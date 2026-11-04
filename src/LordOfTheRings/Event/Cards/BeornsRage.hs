module LordOfTheRings.Event.Cards.BeornsRage (beornsRage) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Window (characterMatches)
import LordOfTheRings.Trait (Trait (Beorning))

newtype BeornsRage = BeornsRage EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

beorning :: (HeroMatcher, AllyMatcher)
beorning = (HeroWithTrait Beorning, AllyWithTrait Beorning)

-- | Response: After a Beorning character is declared as an attacker or defender against an
-- enemy, that enemy gets -2 attack and -2 defense until the end of the phase.
beornsRage :: EventCard BeornsRage
beornsRage =
  eventWith BeornsRage Cards.beornsRage
    $ windowL
    ?~ AnyWindowOf
      [ uncurry (CharacterAttackerDeclaredWindow After) beorning AnyEnemy
      , uncurry (CharacterDefenderDeclaredWindow After) beorning AnyEnemy
      ]

instance RunMessage BeornsRage where
  runMessage msg e@(BeornsRage attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      let declared = [(c, eid) | Window After (DeclaredAsAttacker c eid) <- windows] <> [(c, eid) | Window After (DeclaredAsDefender c eid) <- windows]
      enemies <- map snd <$> filterM (uncurry characterMatches beorning . fst) declared
      chooseOrRunOneM pid do
        questionLabeled "beornsRage.enemy"
        causedBy attrs
        targets (ordNub enemies) \eid -> phaseModifiers attrs eid [AttackModifier (-2), DefenseModifier (-2)]
      pure e
    _ -> BeornsRage <$> runMessage msg attrs
