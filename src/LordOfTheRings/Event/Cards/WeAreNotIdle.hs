module LordOfTheRings.Event.Cards.WeAreNotIdle (weAreNotIdle) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype WeAreNotIdle = WeAreNotIdle EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust X Dwarf heroes to add X resources to a hero's resource pool and draw
-- 1 card. (FAQ 1.9 errata; as printed, X Dwarf characters.)
weAreNotIdle :: EventCard WeAreNotIdle
weAreNotIdle = event WeAreNotIdle Cards.weAreNotIdle

instance RunMessage WeAreNotIdle where
  runMessage msg e@(WeAreNotIdle attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      errata <- playsWithErrata pid
      dwarves <-
        selectCharacters
          (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Dwarf <> ReadyHero)
          (if errata then not_ AnyAlly else AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Dwarf <> ReadyAlly)
      heroes <- select AnyHero
      -- each hero exhausted asks again, until the player is done
      let exhaustMore :: (ReverseQueue n) => Int -> [Target] -> n ()
          exhaustMore x remaining = chooseOneM pid do
            questionLabeledWith "weAreNotIdle.exhaust" [intVar "count" x]
            causedBy attrs
            targets remaining \h -> do
              exhaust h
              exhaustMore (x + 1) (filter (/= h) remaining)
            labeled "weAreNotIdle.done" do
              when (x > 0) $ chooseOneM pid do
                questionLabeledWith "weAreNotIdle.chooseHero" [intVar "count" x]
                causedBy attrs
                targets heroes \h -> placeResources attrs h x
              drawCards attrs pid 1
      exhaustMore 0 dwarves
      pure e
    _ -> WeAreNotIdle <$> runMessage msg attrs
