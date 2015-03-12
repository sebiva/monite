module System.Console.Haskeline.Completion(
                            CompletionFunc,
                            Completion(..),
                            noCompletion,
                            simpleCompletion,
                            -- * Word completion
                            completeWord,
                            completeWordWithPrev,
                            completeQuotedWord,
                            -- * Filename completion
                            completeFilename,
                            listFiles,
                            listPathFiles,
                            filenameWordBreakChars
                        ) where


import System.FilePath
import Data.List(isPrefixOf)
import Control.Monad(forM)

import System.Console.Haskeline.Directory
import System.Console.Haskeline.Monads

-- | Performs completions from the given line state.
--
-- The first 'String' argument is the contents of the line to the left of the cursor,
-- reversed.
-- The second 'String' argument is the contents of the line to the right of the cursor.
--
-- The output 'String' is the unused portion of the left half of the line, reversed.
type CompletionFunc m = (String,String) -> m (String, [Completion])


data Completion = Completion {replacement  :: String, -- ^ Text to insert in line.
                        display  :: String,
                                -- ^ Text to display when listing
                                -- alternatives.
                        isFinished :: Bool
                            -- ^ Whether this word should be followed by a
                            -- space, end quote, etc.
                            }
                    deriving Show

-- | Disable completion altogether.
noCompletion :: Monad m => CompletionFunc m
noCompletion (s,_) = return (s,[])

--------------
-- Word break functions

-- | A custom 'CompletionFunc' which completes the word immediately to the left of the cursor.
--
-- A word begins either at the start of the line or after an unescaped whitespace character.
completeWord :: Monad m => Maybe Char
        -- ^ An optional escape character
        -> [Char]-- ^ Characters which count as whitespace
        -> (String -> m [Completion]) -- ^ Function to produce a list of possible completions
        -> CompletionFunc m
completeWord esc ws = completeWordWithPrev esc ws . const

-- | A custom 'CompletionFunc' which completes the word immediately to the left of the cursor,
-- and takes into account the line contents to the left of the word.
--
-- A word begins either at the start of the line or after an unescaped whitespace character.
completeWordWithPrev :: Monad m => Maybe Char
        -- ^ An optional escape character
        -> [Char]-- ^ Characters which count as whitespace
        -> (String ->  String -> m [Completion])
            -- ^ Function to produce a list of possible completions.  The first argument is the
            -- line contents to the left of the word, reversed.  The second argument is the word
            -- to be completed.
        -> CompletionFunc m
completeWordWithPrev esc ws f (line, _) = do
    let (word,rest) = case esc of
                        Nothing -> break (`elem` ws) line
                        Just e -> escapedBreak e line
    completions <- f rest (reverse word)
    return (rest,map (escapeReplacement esc ws) completions)
  where
    escapedBreak e (c:d:cs) | d == e && c `elem` (e:ws)
            = let (xs,ys) = escapedBreak e cs in (c:xs,ys)
    escapedBreak e (c:cs) | notElem c ws
            = let (xs,ys) = escapedBreak e cs in (c:xs,ys)
    escapedBreak _ cs = ("",cs)

-- | Create a finished completion out of the given word.
simpleCompletion :: String -> Completion
simpleCompletion = completion

-- NOTE: this is the same as for readline, except that I took out the '\\'
-- so they can be used as a path separator.
filenameWordBreakChars :: String
filenameWordBreakChars = " \t\n`@$><=;|&{("

-- A completion command for file and folder names.
completeFilename :: MonadIO m => CompletionFunc m
completeFilename  = completeQuotedWord (Just '\\') "\"'" listFiles
                        $ completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars)
                                listFiles

completion :: String -> Completion
completion str = Completion str str True

setReplacement :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}

escapeReplacement :: Maybe Char -> String -> Completion -> Completion
escapeReplacement esc ws f = case esc of
    Nothing -> f
    Just e -> f {replacement = escape e (replacement f)}
  where
    escape e (c:cs) | c `elem` (e:ws)     = e : c : escape e cs
                    | otherwise = c : escape e cs
    escape _ "" = ""


---------
-- Quoted completion
completeQuotedWord :: Monad m => Maybe Char -- ^ An optional escape character
                            -> [Char] -- ^ Characters which set off quotes
                            -> (String -> m [Completion]) -- ^ Function to produce a list of possible completions
                            -> CompletionFunc m -- ^ Alternate completion to perform if the
                                            -- cursor is not at a quoted word
                            -> CompletionFunc m
completeQuotedWord esc qs completer alterative line@(left,_)
  = case splitAtQuote esc qs left of
    Just (w,rest) | isUnquoted esc qs rest -> do
        cs <- completer (reverse w)
        return (rest, map (addQuotes . escapeReplacement esc qs) cs)
    _ -> alterative line

addQuotes :: Completion -> Completion
addQuotes c = if isFinished c
    then c {replacement = "\"" ++ replacement c ++ "\""}
    else c {replacement = "\"" ++ replacement c}

splitAtQuote :: Maybe Char -> String -> String -> Maybe (String,String)
splitAtQuote esc qs line = case line of
    c:e:cs | isEscape e && isEscapable c
                        -> do
                            (w,rest) <- splitAtQuote esc qs cs
                            return (c:w,rest)
    q:cs   | isQuote q  -> Just ("",cs)
    c:cs                -> do
                            (w,rest) <- splitAtQuote esc qs cs
                            return (c:w,rest)
    ""                  -> Nothing
  where
    isQuote = (`elem` qs)
    isEscape c = Just c == esc
    isEscapable c = isEscape c || isQuote c

isUnquoted :: Maybe Char -> String -> String -> Bool
isUnquoted esc qs s = case splitAtQuote esc qs s of
    Just (_,s') -> not (isUnquoted esc qs s')
    _ -> True


-- | List all of the files or folders beginning with this path.
listFiles :: MonadIO m => FilePath -> m [Completion]
listFiles path = liftIO $ do
    fixedDir <- fixPath dir
    dirExists <- doesDirectoryExist fixedDir
    -- get all of the files in that directory, as basenames
    allFiles <- if not dirExists
                    then return []
                    else fmap (map completion . filterPrefix)
                            $ getDirectoryContents fixedDir
    -- The replacement text should include the directory part, and also
    -- have a trailing slash if it's itself a directory.
    forM allFiles $ \c -> do
            isDir <- doesDirectoryExist (fixedDir </> replacement c)
            return $ setReplacement fullName $ alterIfDir isDir c
  where
    (dir, file) = splitFileName path
    filterPrefix = filter (\f -> notElem f [".",".."]
                                        && file `isPrefixOf` f)
    alterIfDir False c = c
    alterIfDir True c = c {replacement = addTrailingPathSeparator (replacement c),
                            isFinished = False}
    fullName = replaceFileName path

-- turn a user-visible path into an internal version useable by System.FilePath.
fixPath :: String -> IO String
-- For versions of filepath < 1.2
fixPath "" = return "."
fixPath ('~':c:path) | isPathSeparator c = do
    home <- getHomeDirectory
    return (home </> path)
fixPath path = return path


-------------------------------------------------------------------------------
-- EXTENSIONS
--
-- | Given a list of file paths, we construct a list function wich will suggest
-- completions from all these paths.
listPathFiles :: (MonadIO m) => [FilePath] -> (FilePath -> m [Completion])
listPathFiles paths = \path -> liftIO $ do
  let searchPaths = map (++ "/" ++ path) paths
  comps <- mapM listPathFiles' searchPaths
  return (concat comps)

-- | List all of the files or folders beginning with this path.
listPathFiles' :: MonadIO m => FilePath -> m [Completion]
listPathFiles' path = liftIO $ do
    fixedDir <- fixPath dir
    dirExists <- doesDirectoryExist fixedDir
    pathFiles <- if not dirExists
                    then return []
                    else fmap (map completion . filterPrefix)
                            $ getDirectoryContents fixedDir

    let allFiles = pathFiles
    -- The replacement text should include the directory part, and also
    -- have a trailing slash if it's itself a directory.
    forM allFiles $ \c -> return c

  where
    (dir, file) = splitFileName path
    filterPrefix = filter (\f -> notElem f [".",".."]
                                        && file `isPrefixOf` f)

-------------------------------------------------------------------------------
-- Extension :: EDSL for completion functions
--
-- It would be really nice to be able to compose completion functions using
-- smaller components.

-- | A default completion function. This function would have the basic
-- functionality, which is to be able to list completions.
defaultCompletionFunc :: (MonadIO m) => CompletionFunc m
defaultCompletionFunc = undefined

-- | Add a path for the completion function to look for completions in. Adding a
-- path to the completion function would result in that completion function also
-- looking in that path for completions.
--
-- Example 1: addPath "/usr/bin/" defaultCompletionFunc, would result in a
-- completion function that looks for completions in the directory "/usr/bin/"
--
-- Example 2: addPath "./" defaultCompletionFunc, would result in a function
-- with the same functionality as the completeFilename function.
addPath :: (MonadIO m) => FilePath -> CompletionFunc m -> CompletionFunc m
addPath = undefined

-- | Add paths for the completion function to look for completions in. Same as
-- addPath except one would be able to add many paths.
addPaths :: (MonadIO m) => [FilePath] -> CompletionFunc m -> CompletionFunc m
addPaths = undefined

-- | Add a predicate for the files to be considered as completions. This could
-- be really useful, say we want the completion function to only suggest files
-- of type "*.txt", then we could compose such a function with
--
-- Example 1: addPred (\path -> isTxt path) completionFunc, which would result
-- in a completion function that only suggests files of type "*.txt".
--
-- In our case, with the monite shell, we might want to add a predicate that
-- would check that a completion is a binary file in our completion function
-- wich completes binary commands, in order to prevent possible files, or dirs
-- that has been mistakenly placed in the $PATH to be displayed as suggestions.
addPred :: (MonadIO m) => (FilePath -> Bool) -> CompletionFunc m -> CompletionFunc m
addPred = undefined

-- With this simple, albeit useful, interface we could have replaced the
-- listPathFiles function, and constructed our custom completion function as
-- such:
--
-- binCompletion = addPaths binPaths $ addPred (\path -> isBin path)
--                  defaultCompletionFunc
--  where  binPaths = all the paths to the binary files
--         isBin    = checks if a file is a binary file (executable)
