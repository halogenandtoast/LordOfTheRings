module LordOfTheRings.Objective.Cards.BookOfMazarbul (bookOfMazarbul) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype BookOfMazarbul = BookOfMazarbul ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

bookOfMazarbul :: ObjectiveCard BookOfMazarbul
bookOfMazarbul = objective BookOfMazarbul Cards.bookOfMazarbul

-- | Action: Exhaust a hero to claim this objective. Then, attach Book of Mazarbul to that
-- hero. (If detached, return Book of Mazarbul to the staging area.)
instance HasAbilities BookOfMazarbul where
  getAbilities (BookOfMazarbul a) =
    [ anyPlayer $ restricted a 1 (HeroExists $ HeroControlledBy ActivePlayer <> ReadyHero) $ ActionAbility Free
    | a.placement == StagingArea
    ]

-- | Attached hero cannot attack and does not exhaust to commit to a quest.
instance HasModifiersFor BookOfMazarbul where
  getModifiersFor (BookOfMazarbul a) = case a.placement of
    AttachedTo host -> modified a host [CannotAttack, DoesNotExhaustToCommit]
    _ -> pure ()

instance RunMessage BookOfMazarbul where
  runMessage msg o@(BookOfMazarbul attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOneM pid do
        questionLabeled "bookOfMazarbul.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          attachObjective attrs.id h
      pure o
    _ -> BookOfMazarbul <$> runMessage msg attrs
