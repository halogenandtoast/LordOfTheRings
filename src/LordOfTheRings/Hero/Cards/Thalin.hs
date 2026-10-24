module LordOfTheRings.Hero.Cards.Thalin (thalin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Thalin = Thalin HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thalin :: HeroCard Thalin
thalin = hero Thalin Cards.thalin

-- | While Thalin is committed to a quest, deal 1 damage to each enemy as it is revealed by
-- the encounter deck.
--
-- He deals it before its When Revealed and keywords resolve (FAQ errata).
instance HasAbilities Thalin where
  getAbilities (Thalin a) =
    [restricted a 1 (HeroExists $ HeroWithId a.id <> CommittedHero) $ silent $ EnemyRevealedWindow When]

instance RunMessage Thalin where
  runMessage msg h@(Thalin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ (headMay [enemy | Window _ (Revealed enemy@(EnemyTarget _)) <- windows]) \enemy -> dealDamage attrs enemy 1
      pure h
    _ -> Thalin <$> runMessage msg attrs
