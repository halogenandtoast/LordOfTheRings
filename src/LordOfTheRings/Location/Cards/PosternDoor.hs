module LordOfTheRings.Location.Cards.PosternDoor (posternDoor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype PosternDoor = PosternDoor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

posternDoor :: LocationCard PosternDoor
posternDoor = location PosternDoor Cards.posternDoor

-- | While Postern Door is the active location, each hero gets +1 attack.
instance HasModifiersFor PosternDoor where
  getModifiersFor (PosternDoor a) = when (a.placement == ActiveLocation) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [AttackModifier 1]

-- | Forced: When Postern Door is explored, until the end of the round, players cannot
-- declare attacks.
instance HasAbilities PosternDoor where
  getAbilities (PosternDoor a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage PosternDoor where
  runMessage msg l@(PosternDoor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> roundModifier a (PlayerTarget p) CannotAttack
      pure l
    _ -> PosternDoor <$> runMessage msg a
