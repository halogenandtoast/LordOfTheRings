module LordOfTheRings.Enemy.Cards.DreadnaughtPillagers (corsairPlunderer, daringBuccaneer, fierceMarauder) where

import LordOfTheRings.Card (CardType (EventType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype DreadnaughtPillager = DreadnaughtPillager EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairPlunderer, daringBuccaneer, fierceMarauder :: EnemyCard DreadnaughtPillager
corsairPlunderer = enemy DreadnaughtPillager Cards.corsairPlunderer
daringBuccaneer = enemy DreadnaughtPillager Cards.daringBuccaneer
fierceMarauder = enemy DreadnaughtPillager Cards.fierceMarauder

instance RunMessage DreadnaughtPillager where
  runMessage msg e@(DreadnaughtPillager attrs) = case msg of
    Pillaged eid pid cards | eid == attrs.id -> do
      blank <- elem Blank <$> getModifiers attrs
      unless blank $ for_ (filter (cardMatches $ CardWithType EventType) cards) \_ ->
        if
          | toCardCode attrs == toCardCode Cards.corsairPlunderer -> discardNonObjectiveAttachment attrs pid
          | toCardCode attrs == toCardCode Cards.fierceMarauder -> discardResourceFromAHero attrs pid
          | otherwise -> do
              characters <- charactersControlledBy pid
              chooseOrRunOneM pid do
                questionLabeled "dreadnaught.pillageDamage"
                causedBy attrs
                targets characters \c -> dealDamage attrs c 1
      pure e
    _ -> DreadnaughtPillager <$> runMessage msg attrs
