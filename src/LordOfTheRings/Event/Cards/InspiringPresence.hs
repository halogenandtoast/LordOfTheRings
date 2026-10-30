module LordOfTheRings.Event.Cards.InspiringPresence (inspiringPresence) where

import LordOfTheRings.Card (cdThreatCost, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype InspiringPresence = InspiringPresence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

inspiringPresence :: EventCard InspiringPresence
inspiringPresence = event InspiringPresence Cards.inspiringPresence

-- | Action: Choose a hero you control. Each hero with lower threat cost than the chosen
-- hero gets +2 defense until the end of the phase. Then, the players as a group may spend
-- 2 leadership resources to give +2 attack to each hero with lower threat cost than the
-- chosen hero until the end of the phase.
instance RunMessage InspiringPresence where
  runMessage msg e@(InspiringPresence attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let threatCost = fmap (fromMaybe 0 . cdThreatCost . toCardDef) . getHero
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseHero"
        causedBy attrs
        targets heroes \h -> do
          cost <- threatCost h
          lower <- filterM (fmap (< cost) . threatCost) =<< select AnyHero
          for_ lower \x -> phaseModifier attrs x (DefenseModifier 2)
          groupMaySpend pid attrs Leadership 2 $ for_ lower \x -> phaseModifier attrs x (AttackModifier 2)
      pure e
    _ -> InspiringPresence <$> runMessage msg attrs
