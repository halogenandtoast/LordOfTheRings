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
    [ anyPlayer $ mkAbility a 1 $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

-- | Attached hero cannot attack and does not exhaust to commit to a quest.
instance HasModifiersFor BookOfMazarbul where
  getModifiersFor (BookOfMazarbul a) = case a.placement of
    AttachedTo host -> modified a host [CannotAttack, DoesNotExhaustToCommit]
    _ -> pure ()

instance RunMessage BookOfMazarbul where
  runMessage msg o@(BookOfMazarbul attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      -- the hero exhausted to pay
      for_ [h | Window _ (Exhausted h) <- windows] $ attachObjective attrs.id
      pure o
    _ -> BookOfMazarbul <$> runMessage msg attrs
