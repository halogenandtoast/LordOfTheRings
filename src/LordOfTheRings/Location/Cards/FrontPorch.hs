module LordOfTheRings.Location.Cards.FrontPorch (frontPorch) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Trait (Trait (Goblin))

newtype FrontPorch = FrontPorch LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

frontPorch :: LocationCard FrontPorch
frontPorch = location FrontPorch Cards.frontPorch

-- | While Front Porch is the active location, players cannot attack Goblin enemies.
instance HasModifiersFor FrontPorch where
  getModifiersFor (FrontPorch a) = when (a.placement == ActiveLocation) do
    goblins <- select (EnemyWithTrait Goblin)
    for_ goblins \g -> modified a g [CannotBeAttacked]

-- | Action: The first player may spend 2 Baggins resources to treat Front Porch's printed
-- text box as if it were blank until the end of the round.
instance HasAbilities FrontPorch where
  getAbilities (FrontPorch a) =
    [ anyPlayer
        $ restricted a 1 (PlayerExists $ FirstPlayer <> ActivePlayer)
        $ ActionAbility (SpendResourcesCost Baggins 2)
    ]

instance RunMessage FrontPorch where
  runMessage msg l@(FrontPorch a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      roundModifier a a Blank
      pure l
    _ -> FrontPorch <$> runMessage msg a
