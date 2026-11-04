-- | What A Knife in the Dark's cards share: the Ringwraiths stage 1A sets aside, out of
-- play, which its cards shuffle back into the encounter deck, and its resolution.
module LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (
  eitherRingwraithOr,
  winAKnifeInTheDark,
) where

import Data.List (elemIndex)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Target

{- | "Either shuffle 1 out of play Ringwraith into the encounter deck or ...": the player
chooses. An option is offered only if it can be done in full, the other then being a must;
with neither, both are (FAQ 1.44, "must either X or Y"). The Ringwraiths out of play are
those stage 1A set aside.
-}
eitherRingwraithOr
  :: (ReverseQueue m, Targetable source)
  => PlayerId -> source -> Text -> Bool -> Text -> QueueT Message m () -> m ()
eitherRingwraithOr pid source question able other action = do
  ringwraiths <- getSetAside (cardIs Enemies.ringwraith)
  chooseOrRunOneM pid do
    questionLabeled question
    causedBy source
    for_ (take 1 ringwraiths) \c -> labeled "aKnifeInTheDark.shuffleRingwraith" $ shuffleSetAsideIntoEncounterDeck [c]
    when (able || null ringwraiths) $ labeled other action

{- | If there are no Nazgûl enemies in play, the players win the game. In campaign mode the
campaign card's resolution is chosen as they do, the game asking nothing once it is over:
"Starting with the first player, each player may choose one of the following boon cards and
attach it to a hero he controls: Tireless Ranger, Skilled Healer, Valiant Warrior, Noble
Hero. The chosen cards have been earned by the players." Each is Permanent, so it is
recorded on its hero, for its player's seat (The Black Riders rules insert, "Permanent").
-}
winAKnifeInTheDark :: ReverseQueue m => m ()
winAKnifeInTheDark = do
  campaign <- getCampaignLog
  order <- getsGame (.playerOrder)
  for_ campaign \_ ->
    eachPlayerMayTakeOneOf
      "aKnifeInTheDark.chooseBoon"
      Nothing
      [Attachments.tirelessRanger, Attachments.skilledHealer, Attachments.valiantWarrior, Attachments.nobleHero]
      \p boon -> do
        heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "campaign.attachTo"
          targets heroes \h ->
            updateCampaignLog $ AddToPool $ PoolCard (toCardCode boon) (elemIndex p order) (Just $ toCardCode h)
  winGame
