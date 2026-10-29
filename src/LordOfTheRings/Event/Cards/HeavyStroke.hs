module LordOfTheRings.Event.Cards.HeavyStroke (heavyStroke) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (toCardCode, toCardDef)
import LordOfTheRings.Helpers.Character (getAttackStrength)
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype HeavyStroke = HeavyStroke EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | No more than 1 copy of Heavy Stroke can be played each phase, by the whole group.
-- Response: After a Dwarf deals X damage to an enemy during combat, deal an additional
-- X damage to that enemy. (Limit once per phase.) The players choose which attackers'
-- attack beats the enemy's defense, so X is the most a Dwarf among the attackers can
-- deal: its attack, up to the damage the whole attack dealt.
heavyStroke :: EventCard HeavyStroke
heavyStroke =
  eventWith HeavyStroke Cards.heavyStroke \a ->
    a
      & windowL
      ?~ AttackDealtDamageWithTraitWindow After Dwarf
      & criteriaL
      .~ (DuringPhase CombatPhase <> TargetLacksModifier GameTarget (PlayedThisPhase $ toCardCode Cards.heavyStroke))

instance RunMessage HeavyStroke where
  runMessage msg e@(HeavyStroke attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      phaseModifier attrs GameTarget (PlayedThisPhase $ toCardCode Cards.heavyStroke)
      for_ (take 1 [(eid, as, x) | Window _ (AttackDealtDamage eid as x) <- windows]) \(eid, as, x) -> do
        dwarves <- selectCharacters (HeroWithTrait Dwarf) (AllyWithTrait Dwarf)
        enemy <- toCardDef <$> getEnemy eid
        best <- foldr max 0 <$> for (filter (`elem` dwarves) as) \d -> getAttackStrength [d] eid enemy
        dealDamage attrs eid (min best x)
      pure e
    _ -> HeavyStroke <$> runMessage msg attrs
