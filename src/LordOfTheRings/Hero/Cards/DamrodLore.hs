module LordOfTheRings.Hero.Cards.DamrodLore (damrodLore) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Trap))

newtype DamrodLore = DamrodLore HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

damrodLore :: HeroCard DamrodLore
damrodLore = hero DamrodLore Cards.damrodLore

-- | Reduce the cost of the first Trap card you play each round by 1 (to a minimum of 0).
instance HasModifiersFor DamrodLore where
  getModifiersFor (DamrodLore a) = do
    played <- cardsPlayedThisRound
    let trapPlayed = any (\(p, code, _) -> p == a.owner && Trap `member` cdCardTraits (toCardDef code)) played
    unless trapPlayed $ modified a (PlayerTarget a.owner) [ReduceCostOf (CardWithTrait Trap) 1]

-- | Response: After a Trap card you control is attached to an enemy, draw 1 card.
instance HasAbilities DamrodLore where
  getAbilities (DamrodLore a) = [mkAbility a 1 $ response $ AttachedToEnemyWindow After a.owner Trap]

instance RunMessage DamrodLore where
  runMessage msg h@(DamrodLore attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      drawCards attrs pid 1
      pure h
    _ -> DamrodLore <$> runMessage msg attrs
