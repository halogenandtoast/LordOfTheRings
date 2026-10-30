module LordOfTheRings.Event.Cards.Elevenses (elevenses) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype Elevenses = Elevenses EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

questingHobbits :: PlayerId -> (HeroMatcher, AllyMatcher)
questingHobbits pid =
  ( HeroControlledBy (PlayerWithId pid) <> CommittedHero <> HeroWithTrait Hobbit
  , AllyControlledBy (PlayerWithId pid) <> CommittedAlly <> AllyWithTrait Hobbit
  )

-- | Play only after the staging step.
-- Quest Action: Choose X questing Hobbit characters you control. Ready each chosen
-- character and remove them from the quest. Then, reduce your threat by X.
elevenses :: EventCard Elevenses
elevenses =
  eventWith Elevenses Cards.elevenses \a ->
    a
      & criteriaL
      .~ DuringPhase QuestPhase
      <> TargetHasModifier GameTarget AfterStagingStep
      <> uncurry (CharactersAtLeast 1) (questingHobbits a.owner)

instance RunMessage Elevenses where
  runMessage msg e@(Elevenses attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      chooseHobbits attrs pid [] =<< uncurry selectCharacters (questingHobbits pid)
      pure e
    _ -> Elevenses <$> runMessage msg attrs

-- | Each Hobbit is chosen or passed over in turn, and the choosing done once one is chosen.
chooseHobbits :: ReverseQueue m => EventAttrs -> PlayerId -> [Target] -> [Target] -> m ()
chooseHobbits attrs pid chosen remaining = chooseOrRunOneM pid do
  questionLabeled "elevenses.choose"
  causedBy attrs
  unless (null chosen) $ labeled "elevenses.done" do
    for_ chosen \c -> do
      ready c
      removeFromQuest c
    lowerThreatBy attrs pid (length chosen)
  for_ (zip remaining (drop 1 $ tails remaining)) \(c, rest) ->
    targeting c $ chooseHobbits attrs pid (c : chosen) rest
