module LordOfTheRings.Hero.Cards.SarumanLore (sarumanLore) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership, Spirit, Tactics))

newtype SarumanLore = SarumanLore HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

sarumanLore :: HeroCard SarumanLore
sarumanLore = hero SarumanLore Cards.sarumanLore

-- | When a player card effect would reduce your threat by any amount, reduce that amount
-- to 1. While playing an event with the printed doomed keyword, Saruman gains the printed
-- Leadership, Spirit, and Tactics icons.
instance HasModifiersFor SarumanLore where
  getModifiersFor (SarumanLore a) = do
    modified a (PlayerTarget a.owner) [PlayerCardThreatReductionLimit 1]
    modifySelf a [PrintedResourceIconFor (CardWithType EventType <> CardWithDoomed) s | s <- [Leadership, Spirit, Tactics]]

-- | Response: After you play a card with the doomed keyword, ready Saruman.
instance HasAbilities SarumanLore where
  getAbilities (SarumanLore a) =
    [restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero) $ response $ PlayedCardWithDoomedWindow After a.owner]

instance RunMessage SarumanLore where
  runMessage msg h@(SarumanLore attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> SarumanLore <$> runMessage msg attrs
