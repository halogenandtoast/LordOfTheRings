module LordOfTheRings.Location.Cards.RuinedPlaza (ruinedPlaza) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (cardsUnderOrcsSearch)

newtype RuinedPlaza = RuinedPlaza LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinedPlaza :: LocationCard RuinedPlaza
ruinedPlaza = location RuinedPlaza Cards.ruinedPlaza

-- | Forced: After Ruined Plaza enters the staging area, place 1 damage here.
instance HasAbilities RuinedPlaza where
  getAbilities (RuinedPlaza a) = [mkAbility a 1 $ forced $ CardEntersStagingAreaWindow After (toTarget a)]

-- | Shadow: Defending player assigns X damage among characters he controls. X is the
-- number of cards underneath The Orcs' Search. He assigns it 1 at a time, a step of the
-- shadow for each.
instance RunMessage RuinedPlaza where
  runMessage msg l@(RuinedPlaza a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeDamage a a 1
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      n <- cardsUnderOrcsSearch
      doStep n msg
      pure l
    DoStep n (ResolveShadowEffect cid _ pid _) | cid == a.cardId, n > 0 -> do
      characters <- charactersControlledBy pid
      chooseOneM pid do
        questionLabeled "ruinedPlaza.assign"
        causedBy (CardIdTarget cid)
        targets characters \c -> dealDamage (CardIdSource cid) c 1
      doStep (n - 1) msg
      pure l
    _ -> RuinedPlaza <$> runMessage msg a
