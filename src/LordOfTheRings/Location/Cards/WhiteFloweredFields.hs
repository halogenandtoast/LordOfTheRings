module LordOfTheRings.Location.Cards.WhiteFloweredFields (whiteFloweredFields) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)

newtype WhiteFloweredFields = WhiteFloweredFields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

whiteFloweredFields :: LocationCard WhiteFloweredFields
whiteFloweredFields = location WhiteFloweredFields Cards.whiteFloweredFields

-- | Forced: At the end of the round, each player must either exhaust a hero he controls or
-- place 1 progress token on To the Tower.
instance HasAbilities WhiteFloweredFields where
  getAbilities (WhiteFloweredFields a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage WhiteFloweredFields where
  runMessage msg l@(WhiteFloweredFields a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
        chooseOneM pid do
          questionLabeled "whiteFloweredFields.choose"
          causedBy a
          targets heroes exhaust
          labeled "whiteFloweredFields.progress" $ placeOnTower a 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> WhiteFloweredFields <$> runMessage msg a
