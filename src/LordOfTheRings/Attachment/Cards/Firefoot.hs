module LordOfTheRings.Attachment.Cards.Firefoot (firefoot) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Helpers.Character (getAttackStrength)
import LordOfTheRings.Helpers.Staging (getEnemyDefense, getEnemyHitPoints)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Trait (Trait (Rohan))

newtype Firefoot = Firefoot AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

firefoot :: AttachmentCard Firefoot
firefoot =
  attachment Firefoot Cards.firefoot
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithSphere Tactics, HeroWithTrait Rohan])

-- | Attached hero gets +1 attack (+2 attack instead if attached hero is Éomer).
instance HasModifiersFor Firefoot where
  getModifiersFor (Firefoot a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      eomer <- (\(Name title _) -> title == "Éomer") . (.name) <$> getHero hid
      modified a host [AttackModifier $ if eomer then 2 else 1]
    _ -> pure ()

-- | Response: After attached hero attacks alone, exhaust Firefoot to choose a non-unique
-- enemy engaged with you. Excess damage dealt by this attack is assigned to the chosen
-- enemy.
-- ponytail: offered as the lone attack resolves, and only when it has damage to spare,
-- rather than through a window of its own
instance RunMessage Firefoot where
  runMessage msg (Firefoot a) = case msg of
    ResolveAttack pid eid | not (attachmentExhausted a), AttachedTo host <- a.placement -> do
      let you = PlayerWithId pid
      attackers <- selectCharacters (HeroControlledBy you <> AttackingHero) (AllyControlledBy you <> AttackingAlly)
      others <- select (EnemyEngagedWith you <> not_ UniqueEnemy <> not_ (EnemyWithId eid))
      present <- selectAny (EnemyWithId eid)
      when (attackers == [host] && notNull others && present) do
        enemy <- getEnemy eid
        attack <- getAttackStrength attackers eid (toCardDef enemy)
        defense <- getEnemyDefense eid
        hitPoints <- getEnemyHitPoints eid
        let excess = attack - defense - (hitPoints - enemy.damage)
        when (excess > 0) $ chooseOneM pid do
          questionLabeled "firefoot.excess"
          causedBy a
          targets others \e -> do
            exhaust a
            dealDamage (PlayerSource pid) e excess
          labeled "pass" $ pure ()
      Firefoot <$> runMessage msg a
    _ -> Firefoot <$> runMessage msg a
