module LordOfTheRings.Attachment.Cards.Firefoot (firefoot) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Trait (Trait (Rohan))

newtype Firefoot = Firefoot AttachmentAttrs
  deriving anyclass (IsAttachment)
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
instance HasAbilities Firefoot where
  getAbilities (Firefoot a) =
    [ restricted a 1 (exists $ EnemyEngagedWith (PlayerWithId a.owner) <> not_ UniqueEnemy)
        $ ResponseAbility (AttackedAloneWithExcessWindow After host) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage Firefoot where
  runMessage msg s@(Firefoot a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ [(eid, n) | Window _ (AttackedAlone _ eid n) <- windows] \(eid, n) -> do
        others <- select (EnemyEngagedWith (PlayerWithId pid) <> not_ UniqueEnemy <> not_ (EnemyWithId eid))
        chooseOrRunOneM pid do
          questionLabeled "firefoot.excess"
          causedBy a
          targets others \e -> dealDamage (PlayerSource pid) e n
      pure s
    _ -> Firefoot <$> runMessage msg a
