module LordOfTheRings.Ally.Cards.ElrondsChampion (elrondsChampion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype ElrondsChampion = ElrondsChampion AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from The Battle of Carn Dûm.
elrondsChampion :: AllyCard ElrondsChampion
elrondsChampion = ally ElrondsChampion Cards.elrondsChampion

-- | Response: After an enemy engages you, ready Elrond's Champion.
instance HasAbilities ElrondsChampion where
  getAbilities (ElrondsChampion a) =
    [mkAbility a 1 $ response $ PlayerEngagedEnemyWindow After pid | Just pid <- [placementController a.placement]]

instance RunMessage ElrondsChampion where
  runMessage msg s@(ElrondsChampion a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> ElrondsChampion <$> runMessage msg a
