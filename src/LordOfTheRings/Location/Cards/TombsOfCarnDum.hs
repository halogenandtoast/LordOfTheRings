module LordOfTheRings.Location.Cards.TombsOfCarnDum (tombsOfCarnDum) where

import LordOfTheRings.Card (Card (PlayerCard), CardDef (cdCardType), toCardDef)
import LordOfTheRings.Card.CardType (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

newtype TombsOfCarnDum = TombsOfCarnDum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tombsOfCarnDum :: LocationCard TombsOfCarnDum
tombsOfCarnDum = location TombsOfCarnDum Cards.tombsOfCarnDum

-- | Forced: At the end of the round, discard the top card of each player's deck. Each
-- player must reanimate each ally discarded by this effect.
instance HasAbilities TombsOfCarnDum where
  getAbilities (TombsOfCarnDum a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | Travel: Deal 1 shadow card to each Undead enemy in play to travel here.
instance RunMessage TombsOfCarnDum where
  runMessage msg l@(TombsOfCarnDum a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        top <- take 1 . (.deck) <$> getPlayer pid
        discardTopOfDeck pid 1
        for_ [c | PlayerCard c <- top, cdCardType (toCardDef c) == AllyType] \c -> reanimate c.id
      pure l
    TravelTo lid | lid == a.id -> do
      traverse_ dealShadowCard =<< select (EnemyWithTrait Undead)
      pure l
    _ -> TombsOfCarnDum <$> runMessage msg a
