module System.Console.Haskeline.Command.Completion(
                            CompletionFunc,
                            Completion,
                            CompletionType(..),
                            completionCmd
                            ) where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.Key
import System.Console.Haskeline.Term (Layout(..), CommandMonad(..))
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Monads

import Data.List(transpose, unfoldr)

-- | Uses the given completion, and adds the completion to the current
-- InsertMode. If the completion is finished, a " " is appended, otherwise the
-- string is added to the left of the cursor, and it will be possible to keep
-- completing.
useCompletion :: InsertMode -> Completion -> InsertMode
useCompletion im c = insertString r im
    where r | isFinished c = replacement c ++ " "
            | otherwise = replacement c

-- | Ask for the possible completions, will perform runCompletion for the 'm'
-- monad on the current InsertMode, and return a modified InsertMode along with
-- a list of completion suggestions, where runCompletion simply runs the
-- Completion Func.
askIMCompletions :: CommandMonad m =>
            Command m InsertMode (InsertMode, [Completion])
askIMCompletions (IMode xs ys) = do
    (rest, completions) <- lift $ runCompletion (withRev graphemesToString xs,
                                            graphemesToString ys)
    return (IMode (withRev stringToGraphemes rest) ys, completions)
  where
    withRev :: ([a] -> [b]) -> [a] -> [b]
    withRev f = reverse . f . reverse

-- | Create a 'Command' for word completion.
completionCmd :: (MonadState Undo m, CommandMonad m)
                => Key -> KeyCommand m InsertMode InsertMode
completionCmd k = k +> saveForUndo >|> \oldIM -> do
    (rest,cs) <- askIMCompletions oldIM
    case cs of
        [] -> effect RingBell >> return oldIM
        [c] -> setState $ useCompletion rest c
        _ -> presentCompletions k oldIM rest cs

-- | Present the completions in either a menu format, or a list format,
-- depending on the user preferences.
presentCompletions :: (MonadReader Prefs m, MonadReader Layout m)
        => Key -> InsertMode -> InsertMode
            -> [Completion] -> CmdM m InsertMode
presentCompletions k oldIM rest cs = do
    prefs <- ask
    case completionType prefs of
        MenuCompletion -> menuCompletion k (map (useCompletion rest) cs) oldIM
        ListCompletion -> do
            withPartial <- setState $ makePartialCompletion rest cs
            if withPartial /= oldIM
                then return withPartial
                else pagingCompletion k prefs cs withPartial

-- | Given a key and a list of InsertModes create a Command that loops through
-- the InsertModes one by one at each key press.
menuCompletion :: Monad m => Key -> [InsertMode] -> Command m InsertMode InsertMode
menuCompletion k = loop
    where
        loop [] = setState
        loop (c:cs) = change (const c) >|> try (k +> loop cs)

-- | Given an InsertMode and a list of completions, creates an InsertMode with
-- a common prefix of the completions added prepended.
makePartialCompletion :: InsertMode -> [Completion] -> InsertMode
makePartialCompletion im completions = insertString partial im
  where
    partial = foldl1 commonPrefix (map replacement completions)
    commonPrefix (c:cs) (d:ds) | c == d = c : commonPrefix cs ds
    commonPrefix _ _ = ""

-- | Given a key, prefs, and completions it constructs a page completion command
-- with the end-user defined preferences.
pagingCompletion :: MonadReader Layout m => Key -> Prefs
                -> [Completion] -> Command m InsertMode InsertMode
pagingCompletion k prefs completions = \im -> do
        ls <- asks $ makeLines (map display completions)
        let pageAction = do
                askFirst prefs (length completions) $
                            if completionPaging prefs
                                then printPage ls
                                else effect (PrintLines ls)
                setState im
        if listCompletionsImmediately prefs
            then pageAction
            else effect RingBell >> try (k +> const pageAction) im

-- | Given prefs, limit, and cmd construct a command containing a question of whether or not to display all
-- suggestions if the completion limit is reached, otherwise return the given
-- cmd.
-- where the command containing the question returns () if the key 'n' is
-- pressed, and returns cmd if 'y' is pressed.
askFirst :: Monad m => Prefs -> Int -> CmdM m ()
            -> CmdM m ()
askFirst prefs n cmd
    | maybe False (< n) (completionPromptLimit prefs) = do
        _ <- setState (Message () $ "Display all " ++ show n
                                 ++ " possibilities? (y or n)")
        keyChoiceCmdM [
            simpleChar 'y' +> cmd
            , simpleChar 'n' +> return ()
            ]
    | otherwise = cmd

-- | Construct a page completion command which lists the suggestions in
-- 'chunks', and waitning for end-user interaciton.
pageCompletions :: MonadReader Layout m => [String] -> CmdM m ()
pageCompletions [] = return ()
pageCompletions wws@(w:ws) = do
    _ <- setState $ Message () "----More----"
    keyChoiceCmdM [
        simpleChar '\n' +> oneLine
        , simpleKey DownKey +> oneLine
        , simpleChar 'q' +> return ()
        , simpleChar ' ' +> (clearMessage >> printPage wws)
        ]
  where
    oneLine = clearMessage >> effect (PrintLines [w]) >> pageCompletions ws
    clearMessage = effect $ LineChange $ const ([],[])

-- | Construct a command from a list of strings which prints a page of
-- completion suggestions, the layout of the page is defined in the layout
-- record.
printPage :: MonadReader Layout m => [String] -> CmdM m ()
printPage ls = do
    layout <- ask
    let (ps,rest) = splitAt (height layout - 1) ls
    effect $ PrintLines ps
    pageCompletions rest

-----------------------------------------------
-- Splitting the list of completions into lines for paging.
makeLines :: [String] -> Layout -> [String]
makeLines ws layout = let
    minColPad = 2
    printWidth = width layout
    maxLength = min printWidth (maximum (map length ws) + minColPad)
    numCols = printWidth `div` maxLength
    ls = if maxLength >= printWidth
                    then map (: []) ws
                    else splitIntoGroups numCols ws
    in map (padWords maxLength) ls

-- Add spaces to the end of each word so that it takes up the given length.
-- Don't padd the word in the last column, since printing a space in the last column
-- causes a line wrap on some terminals.
padWords :: Int -> [String] -> String
padWords _ [x] = x
padWords _ [] = ""
padWords len (x:xs) = x ++ replicate (len - glength x) ' '
			++ padWords len xs
    where
        -- kludge: compute the length in graphemes, not chars.
        -- but don't use graphemes for the max length, since I'm not convinced
        -- that would work correctly. (This way, the worst that can happen is
        -- that columns are longer than necessary.)
        glength = length . stringToGraphemes

-- Split xs into rows of length n,
-- such that the list increases incrementally along the columns.
-- e.g.: splitIntoGroups 4 [1..11] ==
-- [[1,4,7,10]
-- ,[2,5,8,11]
-- ,[3,6,9]]
splitIntoGroups :: Int -> [a] -> [[a]]
splitIntoGroups n xs = transpose $ unfoldr f xs
    where
        f [] = Nothing
        f ys = Just (splitAt k ys)
        k = ceilDiv (length xs) n

-- ceilDiv m n is the smallest k such that k * n >= m.
ceilDiv :: Integral a => a -> a -> a
ceilDiv m n | m `rem` n == 0    =  m `div` n
            | otherwise         =  m `div` n + 1


