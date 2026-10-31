module LordOfTheRings.Treachery.Cards.RainWashedTracks (rainWashedTracks) where

import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (getMissionObjective, missionResources)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RainWashedTracks = RainWashedTracks TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rainWashedTracks :: TreacheryCard RainWashedTracks
rainWashedTracks = treachery RainWashedTracks Cards.rainWashedTracks

-- | When Revealed: Either remove 1 resource token from the Mission objective, or add 8 to
-- the total threat in the staging area this phase. The first player chooses.
-- Shadow: Either remove 1 resource token from the Mission objective, or attacking enemy
-- gets +3 attack. The defending player chooses.
instance RunMessage RainWashedTracks where
  runMessage msg t@(RainWashedTracks attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseMission lead $ labeled "rainWashedTracks.threat" $ phaseModifier attrs GameTarget (StagingThreatBonus 8)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseMission pid $ labeled "rainWashedTracks.attack" $ enemyGetsAttack attrs eid 3
      pure t
    _ -> RainWashedTracks <$> runMessage msg attrs
   where
    chooseMission pid other = do
      mission <- getMissionObjective
      tokens <- missionResources
      chooseOrRunOneM pid do
        questionLabeled "rainWashedTracks.choose"
        causedBy attrs
        for_ mission \m -> when (tokens > 0) $ labeled "rainWashedTracks.mission" $ removeTokens attrs m Resource 1
        other
