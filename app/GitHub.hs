{-# LANGUAGE DuplicateRecordFields #-}

-- | The few GitHub calls bug reports need, made with the site owner's token for players
-- who have no GitHub account: open an issue, put it on the board, post a comment, read
-- an issue's comments, and read the whole board in one sweep.
module GitHub (
  GitHub,
  githubFromEnv,
  NewIssue (..),
  BoardItem (..),
  Comment (..),
  createIssue,
  addToBoard,
  sweepBoard,
  issueComments,
  postComment,
) where

import Control.Monad (mfilter)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson.Types (Parser, parseEither)
import LordOfTheRings.Prelude
import Network.HTTP.Client (Manager, RequestBody (..), httpLbs, method, parseRequest, requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.Environment (lookupEnv)

-- | Where bugs are filed, as the GitHub flow's new-issue link has it.
owner, repo :: Text
owner = "halogenandtoast"
repo = "LordOfTheRings"

projectNumber :: Int
projectNumber = 3

data GitHub = GitHub {manager :: Manager, token :: Text}

-- | Set up from @LOTR_GITHUB_TOKEN@; without it, bugs can only be filed on GitHub.
githubFromEnv :: Manager -> IO (Maybe GitHub)
githubFromEnv manager = fmap (GitHub manager . pack) . mfilter (not . null) <$> lookupEnv "LOTR_GITHUB_TOKEN"

call :: GitHub -> ByteString -> Text -> Maybe Value -> IO (Either Text LByteString)
call gh verb path body = handleAny (pure . Left . tshow) do
  request <- parseRequest (unpack $ "https://api.github.com" <> path)
  response <-
    httpLbs
      request
        { method = verb
        , requestBody = RequestBodyLBS $ maybe "" encode body
        , requestHeaders =
            [ ("authorization", "Bearer " <> encodeUtf8 gh.token)
            , ("accept", "application/vnd.github+json")
            , ("content-type", "application/json")
            , ("user-agent", "lordoftherings-app")
            , ("x-github-api-version", "2022-11-28")
            ]
        }
      gh.manager
  pure
    if statusIsSuccessful (responseStatus response)
      then Right (responseBody response)
      else Left $ "GitHub said " <> tshow (responseStatus response) <> ": " <> decodeUtf8 (toStrict $ responseBody response)

rest :: (Value -> Parser a) -> GitHub -> ByteString -> Text -> Maybe Value -> IO (Either Text a)
rest parse gh verb path body = (>>= decodeWith parse) <$> call gh verb path body

decodeWith :: (Value -> Parser a) -> LByteString -> Either Text a
decodeWith parse bytes = first pack $ parseEither parse =<< eitherDecode bytes

-- | A GraphQL query's @data@, or its errors.
graphql :: (Value -> Parser a) -> GitHub -> Text -> Value -> IO (Either Text a)
graphql parse gh query variables =
  (>>= decodeWith answer) <$> call gh "POST" "/graphql" (Just $ object ["query" .= query, "variables" .= variables])
 where
  answer = withObject "graphql" \o ->
    o .:? "errors" >>= \case
      Just (errors :: Value) -> fail $ "GitHub GraphQL errors: " <> unpack (decodeUtf8 $ toStrict $ encode errors)
      Nothing -> parse =<< o .: "data"

data NewIssue = NewIssue {number :: Int, url :: Text, nodeId :: Text}

-- | Open an issue, labeled and assigned as the GitHub flow's link asks.
createIssue :: GitHub -> Text -> Text -> [Text] -> IO (Either Text NewIssue)
createIssue gh title body labels =
  rest parse gh "POST" ("/repos/" <> owner <> "/" <> repo <> "/issues")
    $ Just
    $ object ["title" .= title, "body" .= body, "labels" .= labels, "assignees" .= [owner]]
 where
  parse = withObject "issue" \o -> NewIssue <$> o .: "number" <*> o .: "html_url" <*> o .: "node_id"

-- | Put an issue on the project board, where the sweep reads its status.
addToBoard :: GitHub -> Text -> IO (Either Text ())
addToBoard gh contentId = runExceptT do
  projectId <-
    ExceptT
      $ graphql
        (withObject "data" $ (.: "user") >=> withObject "user" ((.: "projectV2") >=> withObject "project" (.: "id")))
        gh
        "query($owner: String!, $number: Int!) { user(login: $owner) { projectV2(number: $number) { id } } }"
        (object ["owner" .= owner, "number" .= projectNumber])
  ExceptT
    $ graphql
      (const $ pure ())
      gh
      "mutation($project: ID!, $content: ID!) { addProjectV2ItemById(input: {projectId: $project, contentId: $content}) { item { id } } }"
      (object ["project" .= (projectId :: Text), "content" .= contentId])

-- | An issue as the board shows it.
data BoardItem = BoardItem
  { number :: Int
  , status :: Maybe Text
  -- ^ the board column
  , closed :: Bool
  , closedReason :: Maybe Text
  -- ^ COMPLETED, NOT_PLANNED or DUPLICATE
  , updatedAt :: UTCTime
  -- ^ moves with each comment, so a sweep can tell which issues' comments to read again
  }

{- | Every issue on the board, a hundred at a time, with who the token belongs to: that
account posts the comments of players without GitHub accounts.
-}
sweepBoard :: GitHub -> IO (Either Text (Text, [BoardItem]))
sweepBoard gh = go Nothing []
 where
  go cursor acc =
    graphql page gh query (object ["owner" .= owner, "number" .= projectNumber, "cursor" .= cursor]) >>= \case
      Left err -> pure $ Left err
      Right (login, items, next) ->
        let acc' = acc <> catMaybes items
         in maybe (pure $ Right (login, acc')) (\c -> go (Just c) acc') next
  query =
    "query($owner: String!, $number: Int!, $cursor: String) {\
    \ viewer { login }\
    \ user(login: $owner) { projectV2(number: $number) {\
    \ items(first: 100, after: $cursor) {\
    \ pageInfo { hasNextPage endCursor }\
    \ nodes {\
    \ fieldValueByName(name: \"Status\") { ... on ProjectV2ItemFieldSingleSelectValue { name } }\
    \ content { ... on Issue { number state stateReason updatedAt repository { nameWithOwner } } }\
    \ } } } } }"
  page :: Value -> Parser (Text, [Maybe BoardItem], Maybe Text)
  page = withObject "data" \o -> do
    login <- o .: "viewer" >>= withObject "viewer" (.: "login")
    items <- o .: "user" >>= withObject "user" (.: "projectV2") >>= withObject "project" (.: "items")
    (nodes, info) <- withObject "items" (\i -> (,) <$> i .: "nodes" <*> i .: "pageInfo") items
    more <- withObject "pageInfo" (.: "hasNextPage") info
    next <- if more then withObject "pageInfo" (.: "endCursor") info else pure Nothing
    (login,,next) <$> traverse item nodes
  item = withObject "item" \o -> do
    status <- o .:? "fieldValueByName" >>= maybe (pure Nothing) (withObject "status" (.:? "name"))
    o .:? "content" >>= \case
      Just (Object c) ->
        c .:? "number" >>= \case
          -- a draft or a pull request
          Nothing -> pure Nothing
          Just number -> do
            here <- c .: "repository" >>= withObject "repository" (.: "nameWithOwner")
            state <- c .: "state"
            if here /= owner <> "/" <> repo
              then pure Nothing
              else
                Just
                  <$> ( BoardItem number status (state == ("CLOSED" :: Text))
                          <$> c .:? "stateReason"
                          <*> c .: "updatedAt"
                      )
      _ -> pure Nothing

data Comment = Comment
  { githubId :: Int
  , author :: Text
  , body :: Text
  , createdAt :: UTCTime
  }

instance FromJSON Comment where
  parseJSON = withObject "comment" \o ->
    Comment
      <$> o .: "id"
      <*> (o .: "user" >>= withObject "user" (.: "login"))
      <*> o .: "body"
      <*> o .: "created_at"

-- | All of an issue's comments, oldest first.
issueComments :: GitHub -> Int -> IO (Either Text [Comment])
issueComments gh n = go 1 []
 where
  go :: Int -> [Comment] -> IO (Either Text [Comment])
  go p acc =
    rest parseJSON gh "GET" (path <> "?per_page=100&page=" <> tshow p) Nothing >>= \case
      Left err -> pure $ Left err
      Right cs | length cs < 100 -> pure $ Right (acc <> cs)
      Right cs -> go (p + 1) (acc <> cs)
  path = "/repos/" <> owner <> "/" <> repo <> "/issues/" <> tshow n <> "/comments"

postComment :: GitHub -> Int -> Text -> IO (Either Text Comment)
postComment gh n body =
  rest parseJSON gh "POST" ("/repos/" <> owner <> "/" <> repo <> "/issues/" <> tshow n <> "/comments")
    $ Just
    $ object ["body" .= body]
