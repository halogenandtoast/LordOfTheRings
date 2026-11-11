{- | Bug reports filed by the site for players without GitHub accounts: what is posted for
them, and the sweep that brings each report's status and comments back from GitHub.

A sweep reads the whole board in one go, then only goes back for the comments of the
issues whose updatedAt has moved since it last read them.
-}
module BugSync (
  issueBody,
  commentBody,
  bugSyncLoop,
) where

import Control.Concurrent (threadDelay)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.Persist.Sql
import GitHub
import LordOfTheRings.Prelude hiding (delete)
import Model
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Keep text posted for a player from mentioning anyone on GitHub: a zero-width space
-- follows each @, and is taken out again as the comment comes back.
quiet, unquiet :: Text -> Text
quiet = T.replace "@" "@\x200B"
unquiet = T.replace "@\x200B" "@"

-- | A player's name as inline code, which no username can break out of.
nameOf :: Text -> Text
nameOf username = "`" <> T.filter (/= '`') username <> "`"

-- | The body of an issue filed for a player, saying who they are and that they will
-- only see replies on the site.
issueBody :: Text -> Bool -> Text -> Text -> Text -> Text
issueBody username blocking description gameUrl fileUrl =
  T.intercalate
    "\n"
    [ "> Filed on the site by " <> nameOf username <> ", who has no GitHub account. They see comments here on the site."
    , "> Blocks their game: **" <> (if blocking then "yes" else "no") <> "**"
    , ""
    , quiet description
    , ""
    , "game: " <> gameUrl
    , "file: " <> fileUrl
    ]

-- | Marks a comment the site's account posted for a player, so it comes back as theirs.
marker :: UserId -> Text
marker userId = "<!-- lotr-user:" <> tshow (fromSqlKey userId) <> " -->"

-- | A comment posted for a player: a header naming them, then what they wrote.
commentBody :: UserId -> Text -> Text -> Text
commentBody userId username body =
  "> " <> nameOf username <> " wrote on the site:\n" <> marker userId <> "\n\n" <> quiet body

{- | What a comment says, and whether the reporter posted it from the site. Only the
site's own account can post one for them: a marker in anyone else's comment is just text.
-}
fromSite :: Text -> UserId -> Comment -> (Bool, Text)
fromSite login reporter comment
  | comment.author == login
  , (_, rest) <- T.breakOn (marker reporter) comment.body
  , not (null rest) =
      (True, T.strip $ unquiet $ drop (length $ marker reporter) rest)
  | otherwise = (False, comment.body)

-- | A report's status: the board's column while open, or why it was closed.
statusOf :: BoardItem -> Text
statusOf item
  | item.closed = "closed:" <> fromMaybe "COMPLETED" item.closedReason
  | otherwise = "board:" <> fromMaybe "" item.status

-- | Sweep every @BUG_SYNC_SECONDS@ (two minutes unless set), for as long as the server runs.
bugSyncLoop :: GitHub -> ConnectionPool -> IO ()
bugSyncLoop gh pool = do
  seconds <- fromMaybe 120 . (>>= readMaybe) <$> lookupEnv "BUG_SYNC_SECONDS"
  forever do
    tryAny (sweep gh pool) >>= \case
      Left err -> complain (tshow err)
      Right errs -> traverse_ complain errs
    threadDelay (max 10 seconds * 1_000_000)
 where
  complain err = TIO.hPutStrLn stderr $ "bug sync: " <> err

sweep :: GitHub -> ConnectionPool -> IO [Text]
sweep gh pool = do
  reports <- runSqlPool (selectList [] []) pool
  if null reports
    then pure []
    else
      sweepBoard gh >>= \case
        Left err -> pure [err]
        Right (login, items) -> do
          let board = mapFromList [(item.number, item) | item <- items] :: Map Int BoardItem
          -- an issue taken off the board is no longer followed: the board is where its
          -- status lives
          fmap catMaybes $ for reports \report ->
            for (lookup (entityVal report).lotrBugReportIssueNumber board) (syncReport gh pool login report)
              <&> join

syncReport :: GitHub -> ConnectionPool -> Text -> Entity LotrBugReport -> BoardItem -> IO (Maybe Text)
syncReport gh pool login (Entity rid report) item = do
  now <- getCurrentTime
  let status = statusOf item
  when (status /= report.lotrBugReportStatus) $ flip runSqlPool pool do
    insert_ $ LotrBugStatusChange rid status now
    update rid
      $ [ LotrBugReportStatus =. status
        , LotrBugReportClearedAt =. if item.closed then report.lotrBugReportClearedAt <|> Just now else Nothing
        ]
      -- the issue first finding its place on the board is not news
      <> [LotrBugReportActivityAt =. now | report.lotrBugReportStatus /= "filed"]
  if report.lotrBugReportSyncedAt == Just item.updatedAt
    then pure Nothing
    else
      issueComments gh item.number >>= \case
        Left err -> pure $ Just err
        Right comments -> do
          flip runSqlPool pool do
            existing <- selectList [LotrBugCommentReportId ==. rid] []
            let known = mapFromList [(c.lotrBugCommentGithubId, e) | e@(Entity _ c) <- existing] :: Map Int (Entity LotrBugComment)
                still = setFromList (map (.githubId) comments) :: Set Int
            for_ existing \(Entity cid c) -> unless (member c.lotrBugCommentGithubId still) $ delete cid
            news <- for comments \comment -> do
              let (theirs, body) = fromSite login report.lotrBugReportUserId comment
              case lookup comment.githubId known of
                Just (Entity cid old) -> do
                  when (old.lotrBugCommentBody /= body) $ update cid [LotrBugCommentBody =. body]
                  pure False
                Nothing -> do
                  inserted <-
                    insertUnique
                      LotrBugComment
                        { lotrBugCommentReportId = rid
                        , lotrBugCommentGithubId = comment.githubId
                        , lotrBugCommentAuthor = comment.author
                        , lotrBugCommentUserId = if theirs then Just report.lotrBugReportUserId else Nothing
                        , lotrBugCommentBody = body
                        , lotrBugCommentCreatedAt = comment.createdAt
                        }
                  -- what the reporter said themselves is nothing new to them
                  pure $ isJust inserted && not theirs
            update rid
              $ [LotrBugReportSyncedAt =. Just item.updatedAt]
              <> [LotrBugReportActivityAt =. now | or news]
          pure Nothing
