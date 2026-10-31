module LordOfTheRings.Attachment.Cards.Silverthorn (silverthorn) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)

newtype Silverthorn = Silverthorn AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign. Attach to a hero. Restricted.
silverthorn :: AttachmentCard Silverthorn
silverthorn = attachment Silverthorn Cards.silverthorn

-- | While attacking or defending against an enemy, attached hero gets +1 attack and +1
-- defense if that enemy shares a Trait with an enemy in the victory display.
instance HasModifiersFor Silverthorn where
  getModifiersFor (Silverthorn a) = case a.placement of
    AttachedTo host -> do
      display <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      let traits = ordNub [t | c <- display, cdCardType (toCardDef c) == EnemyType, t <- toList (cdCardTraits $ toCardDef c)]
      unless (null traits) do
        enemies <- ordNub . concat <$> traverse (select . EnemyWithTrait) traits
        modified a host
          $ [AttackBonusAgainstMatching (EnemyWithId e) 1 | e <- enemies]
          <> [DefenseBonusAgainstMatching (EnemyWithId e) 1 | e <- enemies]
    _ -> pure ()

-- | Response: After attached hero attacks and destroys an enemy, add that enemy to the
-- victory display. (Limit once per game.)
instance HasAbilities Silverthorn where
  getAbilities (Silverthorn a) =
    [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

-- ponytail: the destroyed enemy is the enemy on top of the encounter discard pile, where it
-- has just gone
instance RunMessage Silverthorn where
  runMessage msg s@(Silverthorn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      for_ (take 1 [c | c <- discarded, cdCardType (toCardDef c) == EnemyType]) \c -> addToVictoryDisplay (CardIdTarget c.id)
      pure s
    _ -> Silverthorn <$> runMessage msg a
