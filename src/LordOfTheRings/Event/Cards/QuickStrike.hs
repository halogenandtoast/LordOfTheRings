module LordOfTheRings.Event.Cards.QuickStrike (quickStrike) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype QuickStrike = QuickStrike EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a character you control to immediately declare it as an attacker (and
-- resolve its attack) against any eligible enemy target.
quickStrike :: EventCard QuickStrike
quickStrike =
  eventWith QuickStrike Cards.quickStrike \a ->
    a
      & criteriaL
      .~ ( exists (EnemyEngagedWith $ PlayerWithId a.owner)
            <> AnyCriteria [HeroExists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero), AllyExists (AllyControlledBy (PlayerWithId a.owner) <> ReadyAlly)]
         )

instance RunMessage QuickStrike where
  runMessage msg e@(QuickStrike attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      attackers <- filterM (fmap (\mods -> all (`notElem` mods) [CannotExhaust, CannotAttack, CannotAttackOrDefend]) . getModifiers) =<< selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "quickStrike.exhaust"
        causedBy attrs
        targets attackers \c -> chooseOneM pid do
          questionLabeled "quickStrike.enemy"
          -- FAQ 1.9, p16: Quick Strike targets the character, so enemy immunity does not apply to the attack.
          -- https://images-cdn.fantasyflightgames.com/filer_public/2e/31/2e3129b3-dc51-4c27-81ed-6a72f13e82f3/lotr_faq_19.pdf
          targets enemies \eid -> pushAll $ ExhaustToAttack c eid : declaredAttacker c eid <> [CheckWindows [mkAfter $ AttackersDeclared eid], ResolveAttack pid eid]
      pure e
    _ -> QuickStrike <$> runMessage msg attrs
