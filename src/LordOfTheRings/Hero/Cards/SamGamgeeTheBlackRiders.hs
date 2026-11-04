module LordOfTheRings.Hero.Cards.SamGamgeeTheBlackRiders (samGamgeeTheBlackRiders) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype SamGamgeeTheBlackRiders = SamGamgeeTheBlackRiders HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

samGamgeeTheBlackRiders :: HeroCard SamGamgeeTheBlackRiders
samGamgeeTheBlackRiders = hero SamGamgeeTheBlackRiders Cards.samGamgeeTheBlackRiders

-- | Response: After you engage an enemy with a higher engagement cost than your threat,
-- ready Sam Gamgee. He gets +1 [willpower] +1 [attack] and +1 [defense] until the end of
-- the round. An enemy engaging you is you engaging it (FAQ 1.49).
instance HasAbilities SamGamgeeTheBlackRiders where
  getAbilities (SamGamgeeTheBlackRiders a) =
    [ mkAbility a 1
        $ response
        $ MatchingEnemyEngagedWindow After (EnemyEngagedWith (PlayerWithId a.owner) <> EnemyWithEngagementCostAboveThreatOf a.owner)
    ]

instance RunMessage SamGamgeeTheBlackRiders where
  runMessage msg h@(SamGamgeeTheBlackRiders attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      roundModifiers attrs attrs [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure h
    _ -> SamGamgeeTheBlackRiders <$> runMessage msg attrs
