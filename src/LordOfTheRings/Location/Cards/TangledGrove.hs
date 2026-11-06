module LordOfTheRings.Location.Cards.TangledGrove (tangledGrove) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype TangledGrove = TangledGrove LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

tangledGrove :: LocationCard TangledGrove
tangledGrove = location TangledGrove Cards.tangledGrove

instance HasModifiersFor TangledGrove where
  getModifiersFor (TangledGrove a) = do
    n <- selectCount LocationInStagingArea
    modifySelf a [ThreatModifier n]
    unable <- anyM (\pid -> do
      hand <- discardableHand <$> getPlayer pid
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget pid)
      pure $ null hand || barred) =<< getRemainingPlayers
    when unable $ modifySelf a [CannotBeTraveledTo]

instance RunMessage TangledGrove where
  runMessage msg (TangledGrove a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \pid -> discardRandomFromHand pid 1
      TangledGrove <$> runMessage msg a
    _ -> TangledGrove <$> runMessage msg a
