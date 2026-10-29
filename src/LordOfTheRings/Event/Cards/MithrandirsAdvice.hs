module LordOfTheRings.Event.Cards.MithrandirsAdvice (mithrandirsAdvice) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype MithrandirsAdvice = MithrandirsAdvice EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Draw 1 card for each hero you control with a printed lore resource icon.
mithrandirsAdvice :: EventCard MithrandirsAdvice
mithrandirsAdvice =
  eventWith MithrandirsAdvice Cards.mithrandirsAdvice \a ->
    a & criteriaL .~ exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithPrintedSphere Lore)

instance RunMessage MithrandirsAdvice where
  runMessage msg e@(MithrandirsAdvice attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      n <- length <$> select (HeroControlledBy (PlayerWithId pid) <> HeroWithPrintedSphere Lore)
      drawCards pid n
      pure e
    _ -> MithrandirsAdvice <$> runMessage msg attrs
