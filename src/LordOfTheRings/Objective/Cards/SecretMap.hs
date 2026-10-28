module LordOfTheRings.Objective.Cards.SecretMap (secretMap) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype SecretMap = SecretMap ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

secretMap :: ObjectiveCard SecretMap
secretMap = objective SecretMap Cards.secretMap

-- | Action: Exhaust a hero to claim this objective and attach it to that hero.
-- Action: If attached to a hero, add Secret Map to the victory display to place 3
-- progress tokens on the active location.
instance HasAbilities SecretMap where
  getAbilities (SecretMap a) = case a.placement of
    StagingArea -> [anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)]
    AttachedTo (HeroTarget _) -> [restricted a 2 (exists IsActiveLocation) $ ActionAbility Free]
    _ -> []

instance RunMessage SecretMap where
  runMessage msg o@(SecretMap attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      -- the hero exhausted to pay
      for_ [h | Window _ (Exhausted h) <- windows] $ attachObjective attrs.id
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      addToVictoryDisplay attrs
      traverse_ (\l -> placeProgress attrs l 3) =<< select IsActiveLocation
      pure o
    _ -> SecretMap <$> runMessage msg attrs
