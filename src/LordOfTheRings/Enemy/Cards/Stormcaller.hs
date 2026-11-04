module LordOfTheRings.Enemy.Cards.Stormcaller (stormcaller) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Placement (Placement (StormcallerArea))
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers

newtype Stormcaller = Stormcaller EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stormcaller :: EnemyCard Stormcaller
stormcaller = enemy Stormcaller Cards.stormcaller

{- | Boarding 2. Cannot have attachments.
While the players are not at its stage, the cards in the Stormcaller's area, the Stormcaller
among them, are immune to player card effects (Flight of the Stormcaller rules).
Stages 2C and 3C: If the players are off-course, the Stormcaller gets +2 threat. No more than
4 damage (8 at stage 3C) may be placed on the Stormcaller each round.
-}
-- Campaign Mode only side (the Dream-chaser campaign): 7 attack, 20 hit points, no word
-- against attachments, and: If you are at the Stormcaller's stage, no more than 9 progress
-- can be placed on your quest each round.
-- That side is played as the printed side's card with modifiers bringing it to those
-- values: the board still shows the printed face, as no card def holds the other side.
instance HasModifiersFor Stormcaller where
  getModifiersFor (Stormcaller a) = do
    campaign <- isJust <$> getCampaignLog
    if campaign
      then do
        modifySelf a [AttackModifier 1, HitPointModifier 2]
        whenM atStormcallersStage $ traverse_ (\q -> modified a q [ProgressPerRoundLimit 9]) =<< getMainQuests
      else modifySelf a [CannotHaveAttachments]
    enemies <- select EnemyInStormcallerArea
    for_ enemies \e -> modified a e [ImmuneToPlayerCardEffects]
    locations <- getsGame (filter ((== StormcallerArea) . (.placement)) . toList . (.entities.locations))
    for_ locations \l -> modified a (toTarget l) [ImmuneToPlayerCardEffects]
    stage <- getStormcallerStage
    when (stage `elem` [2, 3]) do
      whenM isOffCourse $ modifySelf a [ThreatModifier 2]
      modifySelf a [DamageLimit $ if stage == 2 then 4 else 8]

{- | When the Stormcaller advances to the next quest stage, return it to the Stormcaller's
area. If the Stormcaller leaves play, the players win the game.
-}
instance RunMessage Stormcaller where
  runMessage msg e@(Stormcaller attrs) = case msg of
    ScenarioSpecific "stormcaller.advanced" _ -> do
      when (attrs.placement /= StormcallerArea) $ placeEnemy attrs.id StormcallerArea
      pure e
    _ | leavesPlay -> do
      winGame
      Stormcaller <$> runMessage msg attrs
    _ -> Stormcaller <$> runMessage msg attrs
   where
    leavesPlay = case msg of
      Destroy _ t -> isTarget attrs t
      Discard t -> isTarget attrs t
      AddToVictoryDisplay t -> isTarget attrs t
      ShuffleIntoEncounterDeck t -> isTarget attrs t
      ReturnToTopOfEncounterDeck t -> isTarget attrs t
      ReturnToBottomOfEncounterDeck t -> isTarget attrs t
      RemoveFromGame t -> isTarget attrs t
      _ -> False
