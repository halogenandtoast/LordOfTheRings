module LordOfTheRings.Attachment.Cards.Roheryn (roheryn) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))
import LordOfTheRings.Trait (Trait (Dunedain))

newtype Roheryn = Roheryn AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain hero.
roheryn :: AttachmentCard Roheryn
roheryn = attachment Roheryn Cards.roheryn & attachTo (AttachToHero $ HeroWithTrait Dunedain)

-- | If the attached hero is Aragorn, he gains a tactics icon.
instance HasModifiersFor Roheryn where
  getModifiersFor (Roheryn a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      aragorn <- selectAny (HeroWithId hid <> HeroWithTitle "Aragorn")
      when aragorn $ modified a host [GainResourceIcon Tactics]
    _ -> pure ()

-- | Response: After you optionally engage an enemy, exhaust Roheryn and attached hero to
-- declare attached hero as an attacker (and resolve its attack) against that enemy.
instance HasAbilities Roheryn where
  getAbilities (Roheryn a) =
    [ mkAbility a 1 $ ResponseAbility (OptionallyEngagedWindow After a.controller) (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage Roheryn where
  runMessage msg s@(Roheryn a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      withHost a \host ->
        for_ (take 1 [eid | Window _ (OptionallyEngaged eid _) <- windows]) \eid -> attackEnemyWith pid a host eid 0
      pure s
    _ -> Roheryn <$> runMessage msg a
