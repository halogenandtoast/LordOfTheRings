module LordOfTheRings.Event.Cards.HeavyStroke (heavyStroke) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype HeavyStroke = HeavyStroke EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a Dwarf deals X damage to an enemy during combat, deal an additional
-- X damage to that enemy. (Limit once per phase.) The damage its attack dealt, with a
-- Dwarf among the attackers.
heavyStroke :: EventCard HeavyStroke
heavyStroke =
  eventWith HeavyStroke Cards.heavyStroke \a ->
    a
      & windowL
      ?~ AttackDealtDamageWithTraitWindow After Dwarf
      & criteriaL
      .~ (DuringPhase CombatPhase <> TargetLacksModifier (PlayerTarget a.owner) (PlayedThisPhase $ toCardCode Cards.heavyStroke))

instance RunMessage HeavyStroke where
  runMessage msg e@(HeavyStroke attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      phaseModifier attrs (PlayerTarget pid) (PlayedThisPhase $ toCardCode Cards.heavyStroke)
      for_ (take 1 [(eid, x) | Window _ (AttackDealtDamage eid _ x) <- windows]) \(eid, x) -> dealDamage attrs eid x
      pure e
    _ -> HeavyStroke <$> runMessage msg attrs
