module LordOfTheRings.Attachment.Cards.Intimidation (intimidation) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype Intimidation = Intimidation AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Treason of Saruman. Permanent, on its card def. Setup: Attach to a Warrior
-- hero: as the Campaign Pool records it on its hero.
intimidation :: AttachmentCard Intimidation
intimidation = attachment Intimidation Cards.intimidation & attachTo (AttachToHero $ HeroWithTrait Warrior)

-- | Response: After attached hero is declared as the only attacker against a non-unique
-- enemy, exhaust Intimidation to give that enemy -2 [defense] for this attack: read once
-- the attackers are all declared.
instance HasAbilities Intimidation where
  getAbilities (Intimidation a) =
    [ restricted a 1 (exists (HeroWithId hid <> AttackingHero) <> NotCriterion (CharactersAtLeast 2 AttackingHero AttackingAlly))
        $ ResponseAbility (MatchingAttackersDeclaredWindow After (NotEnemy UniqueEnemy)) (exhaustCost a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage Intimidation where
  runMessage msg s@(Intimidation a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (AttackersDeclared eid) <- windows]) \eid ->
        attackModifier a eid (DefenseModifier (-2))
      pure s
    _ -> Intimidation <$> runMessage msg a
