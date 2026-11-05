module LordOfTheRings.Treachery.Cards.DarkClouds (darkClouds) where

import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (getGoblinsAssault)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkClouds = DarkClouds TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkClouds :: TreacheryCard DarkClouds
darkClouds = treachery DarkClouds Cards.darkClouds

{- | When Revealed: Each enemy and each location in the staging area gets +1 threat until
the end of the phase. If the players quest unsuccessfully this phase, place 1 progress on
The Goblins' Assault. The lasting effect reaches only the cards in the staging area as it
resolves; the progress waits on the game as 'ProgressIfQuestUnsuccessful'.
Shadow: Discard a non-objective attachment you control.
-}
instance RunMessage DarkClouds where
  runMessage msg t@(DarkClouds attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select EnemyInStagingArea
      locations <- select LocationInStagingArea
      for_ enemies \e -> phaseModifier attrs e (ThreatModifier 1)
      for_ locations \l -> phaseModifier attrs l (ThreatModifier 1)
      getGoblinsAssault >>= traverse_ \oid -> phaseModifier attrs GameTarget (ProgressIfQuestUnsuccessful (ObjectiveTarget oid) 1)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachmentBy (CardIdTarget cid) (CardIdSource cid) pid
      pure t
    _ -> DarkClouds <$> runMessage msg attrs
