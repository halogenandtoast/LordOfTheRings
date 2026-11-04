module LordOfTheRings.Event.Cards.DesperateDefense (desperateDefense) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype DesperateDefense = DesperateDefense EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a sentinel character is declared as a defender, it gets +2 defense for
-- this attack. If this attack deals no damage, ready the defending character.
desperateDefense :: EventCard DesperateDefense
desperateDefense =
  eventWith DesperateDefense Cards.desperateDefense
    $ windowL ?~ CharacterDefenderDeclaredWindow After (HeroWithKeyword Sentinel) (AllyWithKeyword Sentinel) AnyEnemy

instance RunMessage DesperateDefense where
  runMessage msg e@(DesperateDefense attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      sentinels <- flip filterM [(c, eid) | Window _ (DeclaredAsDefender c eid) <- windows] \(c, _) -> case c of
        HeroTarget hid -> selectAny (HeroWithId hid <> HeroWithKeyword Sentinel)
        AllyTarget aid -> selectAny (AllyWithId aid <> AllyWithKeyword Sentinel)
        _ -> pure False
      -- a defender is declared at a time: the window holds the one
      for_ (take 1 sentinels) \(c, eid) -> do
        attackModifier attrs c (DefenseModifier 2)
        readying <- capture (ready c)
        afterAttack eid [IfAttackDealtNoDamage eid readying]
      pure e
    _ -> DesperateDefense <$> runMessage msg attrs
