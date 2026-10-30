module LordOfTheRings.Location.Cards.ElvenCaravel (elvenCaravel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ElvenCaravel = ElvenCaravel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

elvenCaravel :: LocationCard ElvenCaravel
elvenCaravel = location ElvenCaravel Cards.elvenCaravel

-- | Travel: Each player must assign damage among characters he controls equal to the
-- amount of damage on Elven Caravel.
-- Forced: When Elven Caravel is placed underneath The Havens Burn, deal 1 damage to each
-- character in play.
instance RunMessage ElvenCaravel where
  runMessage msg l@(ElvenCaravel a) = case msg of
    TravelTo lid | lid == a.id -> do
      when (a.damage > 0) $ eachPlayer \p -> dealDamageDivided a p a.damage
      pure l
    PlaceUnderneath _ cards | any ((== a.cardId) . (.id)) cards -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> dealDamage a c 1
      ElvenCaravel <$> runMessage msg a
    _ -> ElvenCaravel <$> runMessage msg a
