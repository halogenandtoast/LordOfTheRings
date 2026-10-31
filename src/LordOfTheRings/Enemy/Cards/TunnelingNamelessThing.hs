module LordOfTheRings.Enemy.Cards.TunnelingNamelessThing (tunnelingNamelessThing) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

newtype TunnelingNamelessThing = TunnelingNamelessThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tunnelingNamelessThing :: EnemyCard TunnelingNamelessThing
tunnelingNamelessThing = enemy TunnelingNamelessThing Cards.tunnelingNamelessThing

-- | Forced: After the players travel to a location, Tunneling Nameless Thing engages the
-- first player.
-- If there is no active location, return Tunneling Nameless Thing to the staging area:
-- checked as it engages and as the active location is explored.
instance HasAbilities TunnelingNamelessThing where
  getAbilities (TunnelingNamelessThing a) =
    [ mkAbility a 1 $ forced $ AnyTraveledWindow After
    , mkAbility a 2 $ silent $ EnemyEngagedWindow After a.id
    , mkAbility a 3 $ silent $ ActiveLocationExploredWindow After
    ]

instance RunMessage TunnelingNamelessThing where
  runMessage msg e@(TunnelingNamelessThing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      engageEnemy lead attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) _ _ -> do
      active <- selectAny IsActiveLocation
      case attrs.placement of
        EngagedWith _ | not active -> placeEnemy attrs.id StagingArea
        _ -> pure ()
      pure e
    _ -> TunnelingNamelessThing <$> runMessage msg attrs
