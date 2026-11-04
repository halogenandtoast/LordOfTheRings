module LordOfTheRings.Event.Cards.BraceOfConeys (braceOfConeys) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype BraceOfConeys = BraceOfConeys EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control the Ring-bearer. Planning Action.
braceOfConeys :: EventCard BraceOfConeys
braceOfConeys =
  eventWith BraceOfConeys Cards.braceOfConeys \a ->
    a & criteriaL .~ (DuringPhase PlanningPhase <> exists (ringBearer <> HeroControlledBy (PlayerWithId a.owner)))

data Option = Heal | Resources | Draw | Threat
  deriving stock (Eq, Enum, Bounded)

-- | Add Brace of Coneys to the victory display to (choose 2): heal up to 2 damage from a
-- hero, add 2 resources to a hero's pool, choose a player to draw 2 cards, or reduce a
-- player's threat by 2. Two different ones.
instance RunMessage BraceOfConeys where
  runMessage msg e@(BraceOfConeys attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      addToVictoryDisplay card
      choose pid 2 [minBound .. maxBound]
      pure e
    _ -> BraceOfConeys <$> runMessage msg attrs
   where
    choose :: ReverseQueue n => PlayerId -> Int -> [Option] -> n ()
    choose pid n left = when (n > 0) do
      chooseOneM pid do
        questionLabeled "braceOfConeys.choose"
        causedBy attrs
        for_ left \o -> labeled (label o) do
          resolve pid o
          choose pid (n - 1) (filter (/= o) left)
    label :: Option -> Text
    label = \case
      Heal -> "braceOfConeys.heal"
      Resources -> "braceOfConeys.resources"
      Draw -> "braceOfConeys.draw"
      Threat -> "braceOfConeys.threat"
    resolve :: ReverseQueue n => PlayerId -> Option -> n ()
    resolve pid = \case
      Heal -> do
        heroes <- select AnyHero
        chooseOrRunOneM pid do
          questionLabeled "chooseHero"
          causedBy attrs
          targets heroes \h -> healDamage attrs h 2
      Resources -> do
        heroes <- select AnyHero
        chooseOrRunOneM pid do
          questionLabeled "chooseHero"
          causedBy attrs
          targets heroes \h -> placeResources attrs h 2
      Draw -> choosePlayerM pid attrs \p -> drawCards attrs p 2
      Threat -> choosePlayerM pid attrs \p -> lowerThreatBy attrs p 2
