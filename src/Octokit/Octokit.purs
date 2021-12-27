module Octokit.Octokit
  ( CommitActivity
  , License
  , Octokit
  , RequestParams
  , Response
  , Route(..)
  , SearchResult
  , Repo
  , octokit
  , request
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (class DecodeJson, Json, decodeJson, printJsonDecodeError)
import Data.Either (either)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)

foreign import data Octokit :: Type

foreign import octokit :: String -> Effect Octokit

newtype Route = Route String

derive instance Newtype Route _
derive newtype instance Show Route
derive newtype instance Eq Route

type RequestParams =
  { owner :: String
  , repo :: String
  }

type Response d =
  { status :: Int
  , data :: d
  }

foreign import requestImpl :: forall params. Fn3 Octokit String params (Effect (Promise (Response Json)))

request :: forall params output. DecodeJson output => Octokit -> Route -> params -> Aff (Response output)
request client (Route route) params = runFn3 requestImpl client route params # Promise.toAffE >>= tryParse
  where
  tryParse { status, "data": d } = do
    let
      decoded = decodeJson d
    either (printJsonDecodeError >>> error >>> throwError) (\decodedData -> pure { status, "data": decodedData }) decoded

type CommitActivity = Array
  { days :: Array Int
  , total :: Int
  , week :: Int
  }

type License =
  { key :: Maybe String
  , name :: Maybe String
  , node_id :: Maybe String
  , spdx_id :: Maybe String
  , url :: Maybe String
  }

type Repo =
  { allow_forking :: Maybe Boolean
  , archive_url :: Maybe String
  , archived :: Maybe Boolean
  , assignees_url :: Maybe String
  , blobs_url :: Maybe String
  , branches_url :: Maybe String
  , clone_url :: Maybe String
  , collaborators_url :: Maybe String
  , comments_url :: Maybe String
  , commits_url :: Maybe String
  , compare_url :: Maybe String
  , contents_url :: Maybe String
  , contributors_url :: Maybe String
  , created_at :: Maybe String
  , default_branch :: Maybe String
  , deployments_url :: Maybe String
  , description :: Maybe String
  , disabled :: Maybe Boolean
  , downloads_url :: Maybe String
  , events_url :: Maybe String
  , fork :: Maybe Boolean
  , forks :: Maybe Int
  , forks_count :: Maybe Int
  , forks_url :: Maybe String
  , full_name :: Maybe String
  , git_commits_url :: Maybe String
  , git_refs_url :: Maybe String
  , git_tags_url :: Maybe String
  , git_url :: Maybe String
  , has_downloads :: Maybe Boolean
  , has_issues :: Maybe Boolean
  , has_pages :: Maybe Boolean
  , has_projects :: Maybe Boolean
  , has_wiki :: Maybe Boolean
  , homepage :: Maybe String
  , hooks_url :: Maybe String
  , html_url :: Maybe String
  , id :: Maybe Int
  , is_template :: Maybe Boolean
  , issue_comment_url :: Maybe String
  , issue_events_url :: Maybe String
  , issues_url :: Maybe String
  , keys_url :: Maybe String
  , labels_url :: Maybe String
  , language :: Maybe String
  , languages_url :: Maybe String
  , license :: Maybe License
  , merges_url :: Maybe String
  , milestones_url :: Maybe String
  , mirror_url :: Maybe String
  , name :: Maybe String
  , node_id :: Maybe String
  , notifications_url :: Maybe String
  , open_issues :: Maybe Int
  , open_issues_count :: Maybe Int
  , owner ::
      { avatar_url :: Maybe String
      , events_url :: Maybe String
      , followers_url :: Maybe String
      , following_url :: Maybe String
      , gists_url :: Maybe String
      , gravatar_id :: Maybe String
      , html_url :: Maybe String
      , id :: Maybe Int
      , login :: Maybe String
      , node_id :: Maybe String
      , organizations_url :: Maybe String
      , received_events_url :: Maybe String
      , repos_url :: Maybe String
      , site_admin :: Maybe Boolean
      , starred_url :: Maybe String
      , subscriptions_url :: Maybe String
      , type :: Maybe String
      , url :: Maybe String
      }
  , private :: Maybe Boolean
  , pulls_url :: Maybe String
  , pushed_at :: Maybe String
  , releases_url :: Maybe String
  , score :: Number
  , size :: Maybe Int
  , ssh_url :: Maybe String
  , stargazers_count :: Maybe Int
  , stargazers_url :: Maybe String
  , statuses_url :: Maybe String
  , subscribers_url :: Maybe String
  , subscription_url :: Maybe String
  , svn_url :: Maybe String
  , tags_url :: Maybe String
  , teams_url :: Maybe String
  , topics :: Array String
  , trees_url :: Maybe String
  , updated_at :: Maybe String
  , url :: Maybe String
  , visibility :: Maybe String
  , watchers :: Maybe Int
  , watchers_count :: Maybe Int
  }

type SearchResult =
  { incomplete_results :: Boolean
  , items ::
      Array Repo
  , total_count :: Int
  }
