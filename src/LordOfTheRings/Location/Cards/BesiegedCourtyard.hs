module LordOfTheRings.Location.Cards.BesiegedCourtyard (besiegedCourtyard) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (chooseBattleground, runNightmareBattleground)
import LordOfTheRings.Trait (Trait (Battleground))

newtype BesiegedCourtyard = BesiegedCourtyard LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

besiegedCourtyard :: LocationCard BesiegedCourtyard
besiegedCourtyard = location BesiegedCourtyard Cards.besiegedCourtyard

-- | If Besieged Courtyard has 5 or more damage, discard it and exhaust each ready
-- character.
-- Shadow: Choose one: either attacking enemy makes an additional attack after this one, or
-- deal 10 damage to a Battleground location.
instance RunMessage BesiegedCourtyard where
  runMessage msg l@(BesiegedCourtyard a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      battlegrounds <- select (LocationWithTrait Battleground)
      chooseOneM pid do
        questionLabeled "besiegedCourtyard.choose"
        causedBy a
        labeled "besiegedCourtyard.attack" $ additionalAttack pid eid
        when (notNull battlegrounds) $ labeled "besiegedCourtyard.damage" $ chooseBattleground a pid \b -> dealDamage a b 10
      pure l
    _ -> BesiegedCourtyard <$> runNightmareBattleground exhaustAll msg a
   where
    exhaustAll = do
      heroes <- select ReadyHero
      allies <- select (ReadyAlly <> AllyControlledBy AnyPlayer)
      traverse_ exhaust heroes
      traverse_ exhaust allies
