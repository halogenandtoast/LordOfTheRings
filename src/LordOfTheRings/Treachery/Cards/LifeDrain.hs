module LordOfTheRings.Treachery.Cards.LifeDrain (lifeDrain) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LifeDrain = LifeDrain TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lifeDrain :: TreacheryCard LifeDrain
lifeDrain = treachery LifeDrain Cards.lifeDrain

thaurdir :: EnemyMatcher
thaurdir = EnemyWithCardCode (toCardCode Enemies.thaurdirTheTreacheryOfRhudaur)

-- | Counts as a Condition attachment with the text: "Thaurdir gets +3 hit points."
instance HasModifiersFor LifeDrain where
  getModifiersFor (LifeDrain a) = case a.placement of
    AttachedTo host -> modified a host [HitPointModifier 3]
    _ -> pure ()

-- | When Revealed: Deal Life Drain to Thaurdir, as a shadow card.
-- Shadow: If attacking enemy is Thaurdir, he gets +3 attack. If this attack destroys a
-- character, attach Life Drain to Thaurdir.
-- ponytail: Thaurdir of The Treachery of Rhudaur cannot have attachments, so it stays a
-- shadow card.
instance RunMessage LifeDrain where
  runMessage msg t@(LifeDrain attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select thaurdir
      for_ (take 1 enemies) \eid -> do
        let card = MkEncounterCard attrs.cardId (toCardCode attrs)
        discard attrs
        removeFromEncounterDiscard card
        whileInPlay (EnemyTarget eid) $ push $ AttachShadowCard eid card
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      isThaurdir <- selectAny (EnemyWithId eid <> thaurdir)
      when isThaurdir do
        enemyGetsAttack (CardIdSource cid) eid 3
        ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      barred <- elem CannotHaveAttachments <$> getModifiers eid
      unless barred $ attachShadowTreachery eid cid eid
      pure t
    _ -> LifeDrain <$> runMessage msg attrs
