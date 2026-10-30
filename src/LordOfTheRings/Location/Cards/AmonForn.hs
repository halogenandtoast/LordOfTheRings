module LordOfTheRings.Location.Cards.AmonForn (amonForn) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Orc))

newtype AmonForn = AmonForn LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

amonForn :: LocationCard AmonForn
amonForn = location AmonForn Cards.amonForn

-- | Immune to player card effects.
-- The players cannot win while Amon Forn is in play: Cornered Animals, the stage they win
-- by, holds its progress until it leaves play.
instance HasModifiersFor AmonForn where
  getModifiersFor (AmonForn a) = modified a a [ImmuneToPlayerCardEffects]

-- | While Amon Forn is the active location, it gains: "Forced: After an Orc enemy is added
-- to the staging area, it makes an immediate attack against the first player."
instance HasAbilities AmonForn where
  getAbilities (AmonForn a) =
    [mkAbility a 1 $ forced $ EnemyWithTraitEntersStagingAreaWindow After Orc | a.placement == ActiveLocation]

instance RunMessage AmonForn where
  runMessage msg l@(AmonForn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      lead <- getLead
      let added = [eid | Window _ (EntersStagingArea (EnemyTarget eid)) <- ws]
      orcs <- filterM (\eid -> selectAny (EnemyWithId eid <> EnemyWithTrait Orc)) added
      for_ orcs (immediateAttack lead)
      pure l
    _ -> AmonForn <$> runMessage msg a
