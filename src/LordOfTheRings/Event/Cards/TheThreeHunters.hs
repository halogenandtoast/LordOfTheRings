module LordOfTheRings.Event.Cards.TheThreeHunters (theThreeHunters) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype TheThreeHunters = TheThreeHunters EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control [fellowship] Aragorn. Playable with 3 heroes committed to the
-- quest to choose.
theThreeHunters :: EventCard TheThreeHunters
theThreeHunters =
  eventWith TheThreeHunters Cards.theThreeHunters \a ->
    a
      & criteriaL
      .~ DuringPhase QuestPhase
      <> exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTitle "Aragorn" <> HeroWithSphere Fellowship)
      <> CharactersAtLeast 3 CommittedHero (NotAlly AnyAlly)

-- | Quest Action: Choose 3 heroes committed to the quest. Ready those heroes. Until the end
-- of the round, each of the chosen heroes gets +1 [willpower], +1 [attack], and +1
-- [defense].
instance RunMessage TheThreeHunters where
  runMessage msg e@(TheThreeHunters attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      committed <- select CommittedHero
      let choose :: ReverseQueue m => Int -> [HeroId] -> m ()
          choose 0 _ = pure ()
          choose n left = chooseOneM pid do
            questionLabeled "theThreeHunters.choose"
            causedBy attrs
            targets left \h -> do
              ready h
              roundModifiers attrs h [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
              choose (n - 1) (filter (/= h) left)
      choose (3 :: Int) committed
      pure e
    _ -> TheThreeHunters <$> runMessage msg attrs
