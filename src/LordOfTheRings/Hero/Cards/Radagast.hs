module LordOfTheRings.Hero.Cards.Radagast (radagast) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Creature))

newtype Radagast = Radagast HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

radagast :: HeroCard Radagast
radagast = hero Radagast Cards.radagast

-- | You may use resources from Radagast's pool to pay for Creature allies of any sphere.
instance HasModifiersFor Radagast where
  getModifiersFor (Radagast a) =
    modifySelf a [GainResourceIconFor (CardWithType AllyType <> CardWithTrait Creature) s | s <- [Leadership, Tactics, Spirit, Lore]]

-- | Response: After you play a Creature card, Radagast does not exhaust to quest this
-- round.
instance HasAbilities Radagast where
  getAbilities (Radagast a) =
    [ restricted a 1 (TargetLacksModifier (toTarget a) DoesNotExhaustToCommit)
        $ response
        $ PlayedCardFromHandWindow After a.owner (CardWithTrait Creature)
    ]

instance RunMessage Radagast where
  runMessage msg h@(Radagast attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs DoesNotExhaustToCommit
      pure h
    _ -> Radagast <$> runMessage msg attrs
