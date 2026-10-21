module LordOfTheRings.Event.Cards.OathOfEorl (oathOfEorl) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Gondor, Rohan))

newtype OathOfEorl = OathOfEorl EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Rohan trait and another unique
-- character with the Gondor trait. Response: At the beginning of the combat phase, you
-- resolve the step in which you attack enemies before resolving enemy attacks this phase.
-- ponytail: one character with both traits passes alone; "another" is not checked
oathOfEorl :: EventCard OathOfEorl
oathOfEorl =
  eventWith OathOfEorl Cards.oathOfEorl \a ->
    a
      & (windowL ?~ PhaseBeginsWindow After CombatPhase)
      & (criteriaL .~ (unique Rohan a.owner <> unique Gondor a.owner))
 where
  unique trait pid =
    AnyCriteria
      [ HeroExists (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait trait)
      , AllyExists (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait trait <> UniqueAlly)
      ]

instance RunMessage OathOfEorl where
  runMessage msg e@(OathOfEorl attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      attackBeforeEnemies pid
      pure e
    _ -> OathOfEorl <$> runMessage msg attrs
