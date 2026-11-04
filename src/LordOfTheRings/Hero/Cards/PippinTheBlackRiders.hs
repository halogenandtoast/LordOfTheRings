module LordOfTheRings.Hero.Cards.PippinTheBlackRiders (pippinTheBlackRiders) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype PippinTheBlackRiders = PippinTheBlackRiders HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

pippinTheBlackRiders :: HeroCard PippinTheBlackRiders
pippinTheBlackRiders = hero PippinTheBlackRiders Cards.pippinTheBlackRiders

-- | Each enemy in play gets +1 engagement cost for each Hobbit hero you control.
instance HasModifiersFor PippinTheBlackRiders where
  getModifiersFor (PippinTheBlackRiders a) = do
    hobbits <- length <$> select (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Hobbit)
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [EngagementCostModifier hobbits]

-- | Response: After you engage an enemy with an engagement cost higher than your threat,
-- draw a card. An enemy engaging you is you engaging it (FAQ 1.49).
instance HasAbilities PippinTheBlackRiders where
  getAbilities (PippinTheBlackRiders a) =
    [ mkAbility a 1
        $ response
        $ MatchingEnemyEngagedWindow After (EnemyEngagedWith (PlayerWithId a.owner) <> EnemyWithEngagementCostAboveThreatOf a.owner)
    ]

instance RunMessage PippinTheBlackRiders where
  runMessage msg h@(PippinTheBlackRiders attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      drawCards attrs pid 1
      pure h
    _ -> PippinTheBlackRiders <$> runMessage msg attrs
