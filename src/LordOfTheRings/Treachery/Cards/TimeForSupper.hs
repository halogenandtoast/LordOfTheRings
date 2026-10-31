module LordOfTheRings.Treachery.Cards.TimeForSupper (timeForSupper) where

import LordOfTheRings.Card (CardType (HeroType))
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TimeForSupper = TimeForSupper TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

timeForSupper :: TreacheryCard TimeForSupper
timeForSupper = treachery TimeForSupper Cards.timeForSupper

-- | When Revealed: The first player chooses a hero in any player's discard pile and
-- removes that hero from the game. Then, heal all damage from each Troll enemy in play. If
-- no damage is removed from any Troll enemy as a result of this effect, Time for Supper
-- gains surge.
instance RunMessage TimeForSupper where
  runMessage msg t@(TimeForSupper attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      heroes <- concatForM players \p -> map (p,) . filter ((== HeroType) . (.cardType)) . (.discard) <$> getPlayer p
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "timeForSupper.remove"
        causedBy attrs
        for_ heroes \(p, h) -> targeting h $ removeFromDiscard p h.id
      trolls <- select (EnemyWithTrait Troll)
      damage <- for trolls \e -> (e,) <$> getDamage (toTarget e)
      for_ damage \(e, n) -> when (n > 0) $ healDamage attrs e n
      when (all ((== 0) . snd) damage) revealEncounterCard
      pure t
    _ -> TimeForSupper <$> runMessage msg attrs
