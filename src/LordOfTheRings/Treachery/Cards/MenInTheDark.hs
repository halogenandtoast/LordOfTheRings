module LordOfTheRings.Treachery.Cards.MenInTheDark (menInTheDark) where

import LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (heroPaysOr)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MenInTheDark = MenInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

menInTheDark :: TreacheryCard MenInTheDark
menInTheDark = treachery MenInTheDark Cards.menInTheDark

-- | When Revealed: Each hero must pay 1 resource or take 1 damage. If no hero takes damage
-- from this effect, Men in the Dark gains surge.
-- Shadow: Attacking enemy gets +1 attack for each hero the defending player controls with
-- no resources.
instance RunMessage MenInTheDark where
  runMessage msg t@(MenInTheDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- select AnyHero
      for_ heroes \h -> heroPaysOr attrs h "menInTheDark.takeDamage" $ dealDamage attrs h 1
      doStep 1 msg
      pure t
    -- a damage token on this card counts the heroes that took its damage, as it is dealt:
    -- damage canceled before then is not taken
    DoStep 1 (DealDamage (isSource attrs -> True) (HeroTarget _) n) | n > 0 -> do
      placeTokens attrs attrs Damage 1
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      when (countTokens Damage attrs.treacheryTokens == 0) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      broke <- length <$> select (HeroControlledBy (PlayerWithId pid) <> not_ HeroWithResources)
      enemyGetsAttack attrs eid broke
      pure t
    _ -> MenInTheDark <$> runMessage msg attrs
