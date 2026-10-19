module LordOfTheRings.Event.Cards.StrengthOfWill (strengthOfWill) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype StrengthOfWill = StrengthOfWill EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Response: After you travel to a location, exhaust a Spirit character to place 2
-- progress tokens on that location.
strengthOfWill :: EventCard StrengthOfWill
strengthOfWill =
  eventWith StrengthOfWill Cards.strengthOfWill \a ->
    a
      & windowL ?~ AnyTraveledWindow After
      & criteriaL .~ AnyCriteria [HeroExists (ReadyHero <> HeroWithSphere Spirit), AllyExists (ReadyAlly <> AllyWithSphere Spirit)]

instance RunMessage StrengthOfWill where
  runMessage msg e@(StrengthOfWill attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      spirits <- selectCharacters (ReadyHero <> HeroWithSphere Spirit) (ReadyAlly <> AllyWithSphere Spirit)
      for_ (headMay [lid | Window _ (Traveled lid) <- windows]) \lid ->
        push
          $ questionLabelWith "Exhaust a Spirit character" (Just $ toTarget attrs) [toTarget lid] pid
            [targetLabel c [Exhaust c, PlaceTokens (toSource attrs) (toTarget lid) Progress 2] | c <- spirits]
      pure e
    _ -> StrengthOfWill <$> runMessage msg attrs
