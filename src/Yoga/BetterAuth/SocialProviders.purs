module Yoga.BetterAuth.SocialProviders
  ( SocialProviders
  , SocialProvidersImpl
  , socialProviders
  , BaseProviderImpl
  , ProviderId
  , SignInSocialOptionsImpl
  , SignInSocialResult
  -- Google
  , GoogleProvider
  , GoogleProviderImpl
  , GoogleClientId(..)
  , GoogleClientSecret(..)
  , google
  , google'
  , googleId
  -- GitHub
  , GithubProvider
  , GithubProviderImpl
  , GithubClientId(..)
  , GithubClientSecret(..)
  , github
  , github'
  , githubId
  -- Apple
  , AppleProvider
  , AppleProviderImpl
  , AppleClientId(..)
  , AppleClientSecret(..)
  , apple
  , apple'
  , appleId
  -- Atlassian
  , AtlassianProvider
  , AtlassianProviderImpl
  , AtlassianClientId(..)
  , AtlassianClientSecret(..)
  , atlassian
  , atlassian'
  , atlassianId
  -- Cognito
  , CognitoProvider
  , CognitoProviderImpl
  , CognitoClientId(..)
  , CognitoClientSecret(..)
  , cognito
  , cognito'
  , cognitoId
  -- Discord
  , DiscordProvider
  , DiscordProviderImpl
  , DiscordClientId(..)
  , DiscordClientSecret(..)
  , discord
  , discord'
  , discordId
  -- Dropbox
  , DropboxProvider
  , DropboxProviderImpl
  , DropboxClientId(..)
  , DropboxClientSecret(..)
  , dropbox
  , dropbox'
  , dropboxId
  -- Facebook
  , FacebookProvider
  , FacebookProviderImpl
  , FacebookClientId(..)
  , FacebookClientSecret(..)
  , facebook
  , facebook'
  , facebookId
  -- Figma
  , FigmaProvider
  , FigmaProviderImpl
  , FigmaClientId(..)
  , FigmaClientSecret(..)
  , figma
  , figma'
  , figmaId
  -- GitLab
  , GitlabProvider
  , GitlabProviderImpl
  , GitlabClientId(..)
  , GitlabClientSecret(..)
  , gitlab
  , gitlab'
  , gitlabId
  -- HuggingFace
  , HuggingfaceProvider
  , HuggingfaceProviderImpl
  , HuggingfaceClientId(..)
  , HuggingfaceClientSecret(..)
  , huggingface
  , huggingface'
  , huggingfaceId
  -- Kakao
  , KakaoProvider
  , KakaoProviderImpl
  , KakaoClientId(..)
  , KakaoClientSecret(..)
  , kakao
  , kakao'
  , kakaoId
  -- Kick
  , KickProvider
  , KickProviderImpl
  , KickClientId(..)
  , KickClientSecret(..)
  , kick
  , kick'
  , kickId
  -- Line
  , LineProvider
  , LineProviderImpl
  , LineClientId(..)
  , LineClientSecret(..)
  , line
  , line'
  , lineId
  -- Linear
  , LinearProvider
  , LinearProviderImpl
  , LinearClientId(..)
  , LinearClientSecret(..)
  , linear
  , linear'
  , linearId
  -- LinkedIn
  , LinkedinProvider
  , LinkedinProviderImpl
  , LinkedinClientId(..)
  , LinkedinClientSecret(..)
  , linkedin
  , linkedin'
  , linkedinId
  -- Microsoft
  , MicrosoftProvider
  , MicrosoftProviderImpl
  , MicrosoftClientId(..)
  , MicrosoftClientSecret(..)
  , microsoft
  , microsoft'
  , microsoftId
  -- Naver
  , NaverProvider
  , NaverProviderImpl
  , NaverClientId(..)
  , NaverClientSecret(..)
  , naver
  , naver'
  , naverId
  -- Notion
  , NotionProvider
  , NotionProviderImpl
  , NotionClientId(..)
  , NotionClientSecret(..)
  , notion
  , notion'
  , notionId
  -- Paybin
  , PaybinProvider
  , PaybinProviderImpl
  , PaybinClientId(..)
  , PaybinClientSecret(..)
  , paybin
  , paybin'
  , paybinId
  -- PayPal
  , PaypalProvider
  , PaypalProviderImpl
  , PaypalClientId(..)
  , PaypalClientSecret(..)
  , paypal
  , paypal'
  , paypalId
  -- Polar
  , PolarProvider
  , PolarProviderImpl
  , PolarClientId(..)
  , PolarClientSecret(..)
  , polar
  , polar'
  , polarId
  -- Reddit
  , RedditProvider
  , RedditProviderImpl
  , RedditClientId(..)
  , RedditClientSecret(..)
  , reddit
  , reddit'
  , redditId
  -- Roblox
  , RobloxProvider
  , RobloxProviderImpl
  , RobloxClientId(..)
  , RobloxClientSecret(..)
  , roblox
  , roblox'
  , robloxId
  -- Salesforce
  , SalesforceProvider
  , SalesforceProviderImpl
  , SalesforceClientId(..)
  , SalesforceClientSecret(..)
  , salesforce
  , salesforce'
  , salesforceId
  -- Slack
  , SlackProvider
  , SlackProviderImpl
  , SlackClientId(..)
  , SlackClientSecret(..)
  , slack
  , slack'
  , slackId
  -- Spotify
  , SpotifyProvider
  , SpotifyProviderImpl
  , SpotifyClientId(..)
  , SpotifyClientSecret(..)
  , spotify
  , spotify'
  , spotifyId
  -- TikTok
  , TiktokProvider
  , TiktokProviderImpl
  , TiktokClientId(..)
  , TiktokClientSecret(..)
  , tiktok
  , tiktok'
  , tiktokId
  -- Twitch
  , TwitchProvider
  , TwitchProviderImpl
  , TwitchClientId(..)
  , TwitchClientSecret(..)
  , twitch
  , twitch'
  , twitchId
  -- Twitter
  , TwitterProvider
  , TwitterProviderImpl
  , TwitterClientId(..)
  , TwitterClientSecret(..)
  , twitter
  , twitter'
  , twitterId
  -- Vercel
  , VercelProvider
  , VercelProviderImpl
  , VercelClientId(..)
  , VercelClientSecret(..)
  , vercel
  , vercel'
  , vercelId
  -- VK
  , VkProvider
  , VkProviderImpl
  , VkClientId(..)
  , VkClientSecret(..)
  , vk
  , vk'
  , vkId
  -- Zoom
  , ZoomProvider
  , ZoomProviderImpl
  , ZoomClientId(..)
  , ZoomClientSecret(..)
  , zoom
  , zoom'
  , zoomId
  ) where

import Prelude

import Data.Maybe (Maybe)
import Foreign (Foreign)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

-- | Provider identifier for use with signInSocial
newtype ProviderId = ProviderId String

derive newtype instance Eq ProviderId
derive newtype instance Show ProviderId

-- | Common fields shared by all social provider configurations
type BaseProviderImpl r =
  ( clientId :: String
  , clientSecret :: String
  , redirectURI :: String
  , scope :: Array String
  , disableDefaultScope :: Boolean
  , disableSignUp :: Boolean
  , disableImplicitSignUp :: Boolean
  , overrideUserInfoOnSignIn :: Boolean
  , prompt :: String
  , responseMode :: String
  , enabled :: Boolean
  , disableIdTokenSignIn :: Boolean
  , clientKey :: String
  | r
  )

--------------------------------------------------------------------------------
-- Google
--------------------------------------------------------------------------------

newtype GoogleProvider = GoogleProvider Foreign

type GoogleProviderImpl = BaseProviderImpl
  ( accessType :: String
  , display :: String
  , hd :: String
  )

newtype GoogleClientId = GoogleClientId String
newtype GoogleClientSecret = GoogleClientSecret String

google :: GoogleClientId -> GoogleClientSecret -> GoogleProvider
google (GoogleClientId cid) (GoogleClientSecret csecret) =
  GoogleProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

google' :: forall opts opts_. Union opts opts_ GoogleProviderImpl => { | opts } -> GoogleProvider
google' opts = GoogleProvider (unsafeCoerce opts)

googleId :: ProviderId
googleId = ProviderId "google"

--------------------------------------------------------------------------------
-- GitHub
--------------------------------------------------------------------------------

newtype GithubProvider = GithubProvider Foreign

type GithubProviderImpl = BaseProviderImpl ()

newtype GithubClientId = GithubClientId String
newtype GithubClientSecret = GithubClientSecret String

github :: GithubClientId -> GithubClientSecret -> GithubProvider
github (GithubClientId cid) (GithubClientSecret csecret) =
  GithubProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

github' :: forall opts opts_. Union opts opts_ GithubProviderImpl => { | opts } -> GithubProvider
github' opts = GithubProvider (unsafeCoerce opts)

githubId :: ProviderId
githubId = ProviderId "github"

--------------------------------------------------------------------------------
-- Apple
--------------------------------------------------------------------------------

newtype AppleProvider = AppleProvider Foreign

type AppleProviderImpl = BaseProviderImpl
  ( appBundleIdentifier :: String
  , audience :: String
  )

newtype AppleClientId = AppleClientId String
newtype AppleClientSecret = AppleClientSecret String

apple :: AppleClientId -> AppleClientSecret -> AppleProvider
apple (AppleClientId cid) (AppleClientSecret csecret) =
  AppleProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

apple' :: forall opts opts_. Union opts opts_ AppleProviderImpl => { | opts } -> AppleProvider
apple' opts = AppleProvider (unsafeCoerce opts)

appleId :: ProviderId
appleId = ProviderId "apple"

--------------------------------------------------------------------------------
-- Atlassian
--------------------------------------------------------------------------------

newtype AtlassianProvider = AtlassianProvider Foreign

type AtlassianProviderImpl = BaseProviderImpl ()

newtype AtlassianClientId = AtlassianClientId String
newtype AtlassianClientSecret = AtlassianClientSecret String

atlassian :: AtlassianClientId -> AtlassianClientSecret -> AtlassianProvider
atlassian (AtlassianClientId cid) (AtlassianClientSecret csecret) =
  AtlassianProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

atlassian' :: forall opts opts_. Union opts opts_ AtlassianProviderImpl => { | opts } -> AtlassianProvider
atlassian' opts = AtlassianProvider (unsafeCoerce opts)

atlassianId :: ProviderId
atlassianId = ProviderId "atlassian"

--------------------------------------------------------------------------------
-- Cognito
--------------------------------------------------------------------------------

newtype CognitoProvider = CognitoProvider Foreign

type CognitoProviderImpl = BaseProviderImpl
  ( domain :: String
  , region :: String
  , userPoolId :: String
  , requireClientSecret :: Boolean
  )

newtype CognitoClientId = CognitoClientId String
newtype CognitoClientSecret = CognitoClientSecret String

cognito :: CognitoClientId -> CognitoClientSecret -> CognitoProvider
cognito (CognitoClientId cid) (CognitoClientSecret csecret) =
  CognitoProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

cognito' :: forall opts opts_. Union opts opts_ CognitoProviderImpl => { | opts } -> CognitoProvider
cognito' opts = CognitoProvider (unsafeCoerce opts)

cognitoId :: ProviderId
cognitoId = ProviderId "cognito"

--------------------------------------------------------------------------------
-- Discord
--------------------------------------------------------------------------------

newtype DiscordProvider = DiscordProvider Foreign

type DiscordProviderImpl = BaseProviderImpl
  ( permissions :: Int
  )

newtype DiscordClientId = DiscordClientId String
newtype DiscordClientSecret = DiscordClientSecret String

discord :: DiscordClientId -> DiscordClientSecret -> DiscordProvider
discord (DiscordClientId cid) (DiscordClientSecret csecret) =
  DiscordProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

discord' :: forall opts opts_. Union opts opts_ DiscordProviderImpl => { | opts } -> DiscordProvider
discord' opts = DiscordProvider (unsafeCoerce opts)

discordId :: ProviderId
discordId = ProviderId "discord"

--------------------------------------------------------------------------------
-- Dropbox
--------------------------------------------------------------------------------

newtype DropboxProvider = DropboxProvider Foreign

type DropboxProviderImpl = BaseProviderImpl
  ( accessType :: String
  )

newtype DropboxClientId = DropboxClientId String
newtype DropboxClientSecret = DropboxClientSecret String

dropbox :: DropboxClientId -> DropboxClientSecret -> DropboxProvider
dropbox (DropboxClientId cid) (DropboxClientSecret csecret) =
  DropboxProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

dropbox' :: forall opts opts_. Union opts opts_ DropboxProviderImpl => { | opts } -> DropboxProvider
dropbox' opts = DropboxProvider (unsafeCoerce opts)

dropboxId :: ProviderId
dropboxId = ProviderId "dropbox"

--------------------------------------------------------------------------------
-- Facebook
--------------------------------------------------------------------------------

newtype FacebookProvider = FacebookProvider Foreign

type FacebookProviderImpl = BaseProviderImpl
  ( fields :: Array String
  , configId :: String
  )

newtype FacebookClientId = FacebookClientId String
newtype FacebookClientSecret = FacebookClientSecret String

facebook :: FacebookClientId -> FacebookClientSecret -> FacebookProvider
facebook (FacebookClientId cid) (FacebookClientSecret csecret) =
  FacebookProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

facebook' :: forall opts opts_. Union opts opts_ FacebookProviderImpl => { | opts } -> FacebookProvider
facebook' opts = FacebookProvider (unsafeCoerce opts)

facebookId :: ProviderId
facebookId = ProviderId "facebook"

--------------------------------------------------------------------------------
-- Figma
--------------------------------------------------------------------------------

newtype FigmaProvider = FigmaProvider Foreign

type FigmaProviderImpl = BaseProviderImpl ()

newtype FigmaClientId = FigmaClientId String
newtype FigmaClientSecret = FigmaClientSecret String

figma :: FigmaClientId -> FigmaClientSecret -> FigmaProvider
figma (FigmaClientId cid) (FigmaClientSecret csecret) =
  FigmaProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

figma' :: forall opts opts_. Union opts opts_ FigmaProviderImpl => { | opts } -> FigmaProvider
figma' opts = FigmaProvider (unsafeCoerce opts)

figmaId :: ProviderId
figmaId = ProviderId "figma"

--------------------------------------------------------------------------------
-- GitLab
--------------------------------------------------------------------------------

newtype GitlabProvider = GitlabProvider Foreign

type GitlabProviderImpl = BaseProviderImpl
  ( issuer :: String
  )

newtype GitlabClientId = GitlabClientId String
newtype GitlabClientSecret = GitlabClientSecret String

gitlab :: GitlabClientId -> GitlabClientSecret -> GitlabProvider
gitlab (GitlabClientId cid) (GitlabClientSecret csecret) =
  GitlabProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

gitlab' :: forall opts opts_. Union opts opts_ GitlabProviderImpl => { | opts } -> GitlabProvider
gitlab' opts = GitlabProvider (unsafeCoerce opts)

gitlabId :: ProviderId
gitlabId = ProviderId "gitlab"

--------------------------------------------------------------------------------
-- HuggingFace
--------------------------------------------------------------------------------

newtype HuggingfaceProvider = HuggingfaceProvider Foreign

type HuggingfaceProviderImpl = BaseProviderImpl ()

newtype HuggingfaceClientId = HuggingfaceClientId String
newtype HuggingfaceClientSecret = HuggingfaceClientSecret String

huggingface :: HuggingfaceClientId -> HuggingfaceClientSecret -> HuggingfaceProvider
huggingface (HuggingfaceClientId cid) (HuggingfaceClientSecret csecret) =
  HuggingfaceProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

huggingface' :: forall opts opts_. Union opts opts_ HuggingfaceProviderImpl => { | opts } -> HuggingfaceProvider
huggingface' opts = HuggingfaceProvider (unsafeCoerce opts)

huggingfaceId :: ProviderId
huggingfaceId = ProviderId "huggingface"

--------------------------------------------------------------------------------
-- Kakao
--------------------------------------------------------------------------------

newtype KakaoProvider = KakaoProvider Foreign

type KakaoProviderImpl = BaseProviderImpl ()

newtype KakaoClientId = KakaoClientId String
newtype KakaoClientSecret = KakaoClientSecret String

kakao :: KakaoClientId -> KakaoClientSecret -> KakaoProvider
kakao (KakaoClientId cid) (KakaoClientSecret csecret) =
  KakaoProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

kakao' :: forall opts opts_. Union opts opts_ KakaoProviderImpl => { | opts } -> KakaoProvider
kakao' opts = KakaoProvider (unsafeCoerce opts)

kakaoId :: ProviderId
kakaoId = ProviderId "kakao"

--------------------------------------------------------------------------------
-- Kick
--------------------------------------------------------------------------------

newtype KickProvider = KickProvider Foreign

type KickProviderImpl = BaseProviderImpl ()

newtype KickClientId = KickClientId String
newtype KickClientSecret = KickClientSecret String

kick :: KickClientId -> KickClientSecret -> KickProvider
kick (KickClientId cid) (KickClientSecret csecret) =
  KickProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

kick' :: forall opts opts_. Union opts opts_ KickProviderImpl => { | opts } -> KickProvider
kick' opts = KickProvider (unsafeCoerce opts)

kickId :: ProviderId
kickId = ProviderId "kick"

--------------------------------------------------------------------------------
-- Line
--------------------------------------------------------------------------------

newtype LineProvider = LineProvider Foreign

type LineProviderImpl = BaseProviderImpl ()

newtype LineClientId = LineClientId String
newtype LineClientSecret = LineClientSecret String

line :: LineClientId -> LineClientSecret -> LineProvider
line (LineClientId cid) (LineClientSecret csecret) =
  LineProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

line' :: forall opts opts_. Union opts opts_ LineProviderImpl => { | opts } -> LineProvider
line' opts = LineProvider (unsafeCoerce opts)

lineId :: ProviderId
lineId = ProviderId "line"

--------------------------------------------------------------------------------
-- Linear
--------------------------------------------------------------------------------

newtype LinearProvider = LinearProvider Foreign

type LinearProviderImpl = BaseProviderImpl ()

newtype LinearClientId = LinearClientId String
newtype LinearClientSecret = LinearClientSecret String

linear :: LinearClientId -> LinearClientSecret -> LinearProvider
linear (LinearClientId cid) (LinearClientSecret csecret) =
  LinearProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

linear' :: forall opts opts_. Union opts opts_ LinearProviderImpl => { | opts } -> LinearProvider
linear' opts = LinearProvider (unsafeCoerce opts)

linearId :: ProviderId
linearId = ProviderId "linear"

--------------------------------------------------------------------------------
-- LinkedIn
--------------------------------------------------------------------------------

newtype LinkedinProvider = LinkedinProvider Foreign

type LinkedinProviderImpl = BaseProviderImpl ()

newtype LinkedinClientId = LinkedinClientId String
newtype LinkedinClientSecret = LinkedinClientSecret String

linkedin :: LinkedinClientId -> LinkedinClientSecret -> LinkedinProvider
linkedin (LinkedinClientId cid) (LinkedinClientSecret csecret) =
  LinkedinProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

linkedin' :: forall opts opts_. Union opts opts_ LinkedinProviderImpl => { | opts } -> LinkedinProvider
linkedin' opts = LinkedinProvider (unsafeCoerce opts)

linkedinId :: ProviderId
linkedinId = ProviderId "linkedin"

--------------------------------------------------------------------------------
-- Microsoft
--------------------------------------------------------------------------------

newtype MicrosoftProvider = MicrosoftProvider Foreign

type MicrosoftProviderImpl = BaseProviderImpl
  ( tenantId :: String
  , authority :: String
  , profilePhotoSize :: Int
  , disableProfilePhoto :: Boolean
  )

newtype MicrosoftClientId = MicrosoftClientId String
newtype MicrosoftClientSecret = MicrosoftClientSecret String

microsoft :: MicrosoftClientId -> MicrosoftClientSecret -> MicrosoftProvider
microsoft (MicrosoftClientId cid) (MicrosoftClientSecret csecret) =
  MicrosoftProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

microsoft' :: forall opts opts_. Union opts opts_ MicrosoftProviderImpl => { | opts } -> MicrosoftProvider
microsoft' opts = MicrosoftProvider (unsafeCoerce opts)

microsoftId :: ProviderId
microsoftId = ProviderId "microsoft"

--------------------------------------------------------------------------------
-- Naver
--------------------------------------------------------------------------------

newtype NaverProvider = NaverProvider Foreign

type NaverProviderImpl = BaseProviderImpl ()

newtype NaverClientId = NaverClientId String
newtype NaverClientSecret = NaverClientSecret String

naver :: NaverClientId -> NaverClientSecret -> NaverProvider
naver (NaverClientId cid) (NaverClientSecret csecret) =
  NaverProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

naver' :: forall opts opts_. Union opts opts_ NaverProviderImpl => { | opts } -> NaverProvider
naver' opts = NaverProvider (unsafeCoerce opts)

naverId :: ProviderId
naverId = ProviderId "naver"

--------------------------------------------------------------------------------
-- Notion
--------------------------------------------------------------------------------

newtype NotionProvider = NotionProvider Foreign

type NotionProviderImpl = BaseProviderImpl ()

newtype NotionClientId = NotionClientId String
newtype NotionClientSecret = NotionClientSecret String

notion :: NotionClientId -> NotionClientSecret -> NotionProvider
notion (NotionClientId cid) (NotionClientSecret csecret) =
  NotionProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

notion' :: forall opts opts_. Union opts opts_ NotionProviderImpl => { | opts } -> NotionProvider
notion' opts = NotionProvider (unsafeCoerce opts)

notionId :: ProviderId
notionId = ProviderId "notion"

--------------------------------------------------------------------------------
-- Paybin
--------------------------------------------------------------------------------

newtype PaybinProvider = PaybinProvider Foreign

type PaybinProviderImpl = BaseProviderImpl
  ( issuer :: String
  )

newtype PaybinClientId = PaybinClientId String
newtype PaybinClientSecret = PaybinClientSecret String

paybin :: PaybinClientId -> PaybinClientSecret -> PaybinProvider
paybin (PaybinClientId cid) (PaybinClientSecret csecret) =
  PaybinProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

paybin' :: forall opts opts_. Union opts opts_ PaybinProviderImpl => { | opts } -> PaybinProvider
paybin' opts = PaybinProvider (unsafeCoerce opts)

paybinId :: ProviderId
paybinId = ProviderId "paybin"

--------------------------------------------------------------------------------
-- PayPal
--------------------------------------------------------------------------------

newtype PaypalProvider = PaypalProvider Foreign

type PaypalProviderImpl = BaseProviderImpl
  ( environment :: String
  , requestShippingAddress :: Boolean
  )

newtype PaypalClientId = PaypalClientId String
newtype PaypalClientSecret = PaypalClientSecret String

paypal :: PaypalClientId -> PaypalClientSecret -> PaypalProvider
paypal (PaypalClientId cid) (PaypalClientSecret csecret) =
  PaypalProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

paypal' :: forall opts opts_. Union opts opts_ PaypalProviderImpl => { | opts } -> PaypalProvider
paypal' opts = PaypalProvider (unsafeCoerce opts)

paypalId :: ProviderId
paypalId = ProviderId "paypal"

--------------------------------------------------------------------------------
-- Polar
--------------------------------------------------------------------------------

newtype PolarProvider = PolarProvider Foreign

type PolarProviderImpl = BaseProviderImpl ()

newtype PolarClientId = PolarClientId String
newtype PolarClientSecret = PolarClientSecret String

polar :: PolarClientId -> PolarClientSecret -> PolarProvider
polar (PolarClientId cid) (PolarClientSecret csecret) =
  PolarProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

polar' :: forall opts opts_. Union opts opts_ PolarProviderImpl => { | opts } -> PolarProvider
polar' opts = PolarProvider (unsafeCoerce opts)

polarId :: ProviderId
polarId = ProviderId "polar"

--------------------------------------------------------------------------------
-- Reddit
--------------------------------------------------------------------------------

newtype RedditProvider = RedditProvider Foreign

type RedditProviderImpl = BaseProviderImpl
  ( duration :: String
  )

newtype RedditClientId = RedditClientId String
newtype RedditClientSecret = RedditClientSecret String

reddit :: RedditClientId -> RedditClientSecret -> RedditProvider
reddit (RedditClientId cid) (RedditClientSecret csecret) =
  RedditProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

reddit' :: forall opts opts_. Union opts opts_ RedditProviderImpl => { | opts } -> RedditProvider
reddit' opts = RedditProvider (unsafeCoerce opts)

redditId :: ProviderId
redditId = ProviderId "reddit"

--------------------------------------------------------------------------------
-- Roblox
--------------------------------------------------------------------------------

newtype RobloxProvider = RobloxProvider Foreign

type RobloxProviderImpl = BaseProviderImpl ()

newtype RobloxClientId = RobloxClientId String
newtype RobloxClientSecret = RobloxClientSecret String

roblox :: RobloxClientId -> RobloxClientSecret -> RobloxProvider
roblox (RobloxClientId cid) (RobloxClientSecret csecret) =
  RobloxProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

roblox' :: forall opts opts_. Union opts opts_ RobloxProviderImpl => { | opts } -> RobloxProvider
roblox' opts = RobloxProvider (unsafeCoerce opts)

robloxId :: ProviderId
robloxId = ProviderId "roblox"

--------------------------------------------------------------------------------
-- Salesforce
--------------------------------------------------------------------------------

newtype SalesforceProvider = SalesforceProvider Foreign

type SalesforceProviderImpl = BaseProviderImpl
  ( environment :: String
  , loginUrl :: String
  )

newtype SalesforceClientId = SalesforceClientId String
newtype SalesforceClientSecret = SalesforceClientSecret String

salesforce :: SalesforceClientId -> SalesforceClientSecret -> SalesforceProvider
salesforce (SalesforceClientId cid) (SalesforceClientSecret csecret) =
  SalesforceProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

salesforce' :: forall opts opts_. Union opts opts_ SalesforceProviderImpl => { | opts } -> SalesforceProvider
salesforce' opts = SalesforceProvider (unsafeCoerce opts)

salesforceId :: ProviderId
salesforceId = ProviderId "salesforce"

--------------------------------------------------------------------------------
-- Slack
--------------------------------------------------------------------------------

newtype SlackProvider = SlackProvider Foreign

type SlackProviderImpl = BaseProviderImpl ()

newtype SlackClientId = SlackClientId String
newtype SlackClientSecret = SlackClientSecret String

slack :: SlackClientId -> SlackClientSecret -> SlackProvider
slack (SlackClientId cid) (SlackClientSecret csecret) =
  SlackProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

slack' :: forall opts opts_. Union opts opts_ SlackProviderImpl => { | opts } -> SlackProvider
slack' opts = SlackProvider (unsafeCoerce opts)

slackId :: ProviderId
slackId = ProviderId "slack"

--------------------------------------------------------------------------------
-- Spotify
--------------------------------------------------------------------------------

newtype SpotifyProvider = SpotifyProvider Foreign

type SpotifyProviderImpl = BaseProviderImpl ()

newtype SpotifyClientId = SpotifyClientId String
newtype SpotifyClientSecret = SpotifyClientSecret String

spotify :: SpotifyClientId -> SpotifyClientSecret -> SpotifyProvider
spotify (SpotifyClientId cid) (SpotifyClientSecret csecret) =
  SpotifyProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

spotify' :: forall opts opts_. Union opts opts_ SpotifyProviderImpl => { | opts } -> SpotifyProvider
spotify' opts = SpotifyProvider (unsafeCoerce opts)

spotifyId :: ProviderId
spotifyId = ProviderId "spotify"

--------------------------------------------------------------------------------
-- TikTok
--------------------------------------------------------------------------------

newtype TiktokProvider = TiktokProvider Foreign

type TiktokProviderImpl =
  ( clientKey :: String
  , clientSecret :: String
  , redirectURI :: String
  , scope :: Array String
  , disableDefaultScope :: Boolean
  , disableSignUp :: Boolean
  , disableImplicitSignUp :: Boolean
  , overrideUserInfoOnSignIn :: Boolean
  , prompt :: String
  , responseMode :: String
  , enabled :: Boolean
  , disableIdTokenSignIn :: Boolean
  )

newtype TiktokClientId = TiktokClientId String
newtype TiktokClientSecret = TiktokClientSecret String

tiktok :: TiktokClientId -> TiktokClientSecret -> TiktokProvider
tiktok (TiktokClientId cid) (TiktokClientSecret csecret) =
  TiktokProvider (unsafeCoerce { clientKey: cid, clientSecret: csecret })

tiktok' :: forall opts opts_. Union opts opts_ TiktokProviderImpl => { | opts } -> TiktokProvider
tiktok' opts = TiktokProvider (unsafeCoerce opts)

tiktokId :: ProviderId
tiktokId = ProviderId "tiktok"

--------------------------------------------------------------------------------
-- Twitch
--------------------------------------------------------------------------------

newtype TwitchProvider = TwitchProvider Foreign

type TwitchProviderImpl = BaseProviderImpl
  ( claims :: Array String
  )

newtype TwitchClientId = TwitchClientId String
newtype TwitchClientSecret = TwitchClientSecret String

twitch :: TwitchClientId -> TwitchClientSecret -> TwitchProvider
twitch (TwitchClientId cid) (TwitchClientSecret csecret) =
  TwitchProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

twitch' :: forall opts opts_. Union opts opts_ TwitchProviderImpl => { | opts } -> TwitchProvider
twitch' opts = TwitchProvider (unsafeCoerce opts)

twitchId :: ProviderId
twitchId = ProviderId "twitch"

--------------------------------------------------------------------------------
-- Twitter
--------------------------------------------------------------------------------

newtype TwitterProvider = TwitterProvider Foreign

type TwitterProviderImpl = BaseProviderImpl ()

newtype TwitterClientId = TwitterClientId String
newtype TwitterClientSecret = TwitterClientSecret String

twitter :: TwitterClientId -> TwitterClientSecret -> TwitterProvider
twitter (TwitterClientId cid) (TwitterClientSecret csecret) =
  TwitterProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

twitter' :: forall opts opts_. Union opts opts_ TwitterProviderImpl => { | opts } -> TwitterProvider
twitter' opts = TwitterProvider (unsafeCoerce opts)

twitterId :: ProviderId
twitterId = ProviderId "twitter"

--------------------------------------------------------------------------------
-- Vercel
--------------------------------------------------------------------------------

newtype VercelProvider = VercelProvider Foreign

type VercelProviderImpl = BaseProviderImpl ()

newtype VercelClientId = VercelClientId String
newtype VercelClientSecret = VercelClientSecret String

vercel :: VercelClientId -> VercelClientSecret -> VercelProvider
vercel (VercelClientId cid) (VercelClientSecret csecret) =
  VercelProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

vercel' :: forall opts opts_. Union opts opts_ VercelProviderImpl => { | opts } -> VercelProvider
vercel' opts = VercelProvider (unsafeCoerce opts)

vercelId :: ProviderId
vercelId = ProviderId "vercel"

--------------------------------------------------------------------------------
-- VK
--------------------------------------------------------------------------------

newtype VkProvider = VkProvider Foreign

type VkProviderImpl = BaseProviderImpl
  ( scheme :: String
  )

newtype VkClientId = VkClientId String
newtype VkClientSecret = VkClientSecret String

vk :: VkClientId -> VkClientSecret -> VkProvider
vk (VkClientId cid) (VkClientSecret csecret) =
  VkProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

vk' :: forall opts opts_. Union opts opts_ VkProviderImpl => { | opts } -> VkProvider
vk' opts = VkProvider (unsafeCoerce opts)

vkId :: ProviderId
vkId = ProviderId "vk"

--------------------------------------------------------------------------------
-- Zoom
--------------------------------------------------------------------------------

newtype ZoomProvider = ZoomProvider Foreign

type ZoomProviderImpl = BaseProviderImpl
  ( pkce :: Boolean
  )

newtype ZoomClientId = ZoomClientId String
newtype ZoomClientSecret = ZoomClientSecret String

zoom :: ZoomClientId -> ZoomClientSecret -> ZoomProvider
zoom (ZoomClientId cid) (ZoomClientSecret csecret) =
  ZoomProvider (unsafeCoerce { clientId: cid, clientSecret: csecret })

zoom' :: forall opts opts_. Union opts opts_ ZoomProviderImpl => { | opts } -> ZoomProvider
zoom' opts = ZoomProvider (unsafeCoerce opts)

zoomId :: ProviderId
zoomId = ProviderId "zoom"

--------------------------------------------------------------------------------
-- SocialProviders record
--------------------------------------------------------------------------------

newtype SocialProviders = SocialProviders Foreign

type SocialProvidersImpl =
  ( google :: GoogleProvider
  , github :: GithubProvider
  , apple :: AppleProvider
  , atlassian :: AtlassianProvider
  , cognito :: CognitoProvider
  , discord :: DiscordProvider
  , dropbox :: DropboxProvider
  , facebook :: FacebookProvider
  , figma :: FigmaProvider
  , gitlab :: GitlabProvider
  , huggingface :: HuggingfaceProvider
  , kakao :: KakaoProvider
  , kick :: KickProvider
  , line :: LineProvider
  , linear :: LinearProvider
  , linkedin :: LinkedinProvider
  , microsoft :: MicrosoftProvider
  , naver :: NaverProvider
  , notion :: NotionProvider
  , paybin :: PaybinProvider
  , paypal :: PaypalProvider
  , polar :: PolarProvider
  , reddit :: RedditProvider
  , roblox :: RobloxProvider
  , salesforce :: SalesforceProvider
  , slack :: SlackProvider
  , spotify :: SpotifyProvider
  , tiktok :: TiktokProvider
  , twitch :: TwitchProvider
  , twitter :: TwitterProvider
  , vercel :: VercelProvider
  , vk :: VkProvider
  , zoom :: ZoomProvider
  )

socialProviders :: forall r r_. Union r r_ SocialProvidersImpl => { | r } -> SocialProviders
socialProviders opts = SocialProviders (unsafeCoerce opts)

--------------------------------------------------------------------------------
-- signInSocial types
--------------------------------------------------------------------------------

type SignInSocialOptionsImpl =
  ( callbackURL :: String
  , newUserCallbackURL :: String
  , errorCallbackURL :: String
  , disableRedirect :: Boolean
  , scopes :: Array String
  , requestSignUp :: Boolean
  , loginHint :: String
  )

type SignInSocialResult =
  { url :: Maybe String
  , redirect :: Boolean
  }
