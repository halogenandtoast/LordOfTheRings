module LordOfTheRings.Enemy.Cards.RiderOfRhun (riderOfRhun) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Easterling))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype RiderOfRhun = RiderOfRhun EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

riderOfRhun :: EnemyCard RiderOfRhun
riderOfRhun = enemy RiderOfRhun Cards.riderOfRhun

-- | Archery 2.
-- Rider of Rhûn gets -10 engagement cost for each Easterling attachment it has.
instance HasModifiersFor RiderOfRhun where
  getModifiersFor (RiderOfRhun a) = do
    n <- length <$> attachedWithTrait Easterling (toTarget a)
    when (n > 0) $ modifySelf a [EngagementCostModifier (-10 * n)]

-- | When Revealed: Search the encounter deck and discard pile for an Easterling Horse and
-- attach it to Rider of Rhûn. Shuffle the encounter deck.
instance RunMessage RiderOfRhun where
  runMessage msg e@(RiderOfRhun attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      discards <- getEncounterDiscard
      let horse = find (cardMatches $ cardIs Treacheries.easterlingHorse)
      for_ (horse deck <|> horse discards) \c -> attachFromEncounterDiscard c attrs
      shuffleEncounterDeck
      pure e
    _ -> RiderOfRhun <$> runMessage msg attrs
