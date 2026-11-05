module LordOfTheRings.Location.Cards.Orthanc (orthanc) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Orthanc = Orthanc LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

orthanc :: LocationCard Orthanc
orthanc = location Orthanc Cards.orthanc

-- | Immune to player card effects.
instance HasModifiersFor Orthanc where
  getModifiersFor (Orthanc a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | While Orthanc is in the staging area, it gains: "Forced: After the players quest
-- successfully, the first player looks at the top 3 cards of the encounter deck, reveals
-- 1, and discards the others."
instance HasAbilities Orthanc where
  getAbilities (Orthanc a) = [mkAbility a 1 $ forced $ QuestedSuccessfullyWindow After | a.placement == StagingArea]

instance RunMessage Orthanc where
  runMessage msg l@(Orthanc a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      revealOneOfTopEncounterCards lead a 3
      pure l
    _ -> Orthanc <$> runMessage msg a
