module LordOfTheRings.Treachery.Cards.WhispersInTheDark (whispersInTheDark) where

import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOr)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WhispersInTheDark = WhispersInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
whispersInTheDark :: TreacheryCard WhispersInTheDark
whispersInTheDark = treachery WhispersInTheDark Cards.whispersInTheDark

-- | When Revealed: Either attach a set aside Overcome by Fear to your threat dial, or each
-- character you control gets -1 willpower, -1 attack, and -1 defense until the end of the
-- round.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if undefended).
instance RunMessage WhispersInTheDark where
  runMessage msg t@(WhispersInTheDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      characters <- selectCharacters (HeroControlledBy $ PlayerWithId revealer) (AllyControlledBy $ PlayerWithId revealer)
      attachOvercomeByFearOr attrs revealer "whispersInTheDark" True
        $ for_ characters \c -> roundModifiers attrs c [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)]
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid (ifUndefended defenders 2 1)
      pure t
    _ -> WhispersInTheDark <$> runMessage msg attrs
