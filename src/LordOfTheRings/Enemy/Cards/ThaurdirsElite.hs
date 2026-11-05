module LordOfTheRings.Enemy.Cards.ThaurdirsElite (thaurdirsElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ThaurdirsElite = ThaurdirsElite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thaurdirsElite :: EnemyCard ThaurdirsElite
thaurdirsElite = enemy ThaurdirsElite Cards.thaurdirsElite

-- | Forced: If there are 3 shadow cards dealt to Thaurdir's Elite, discard all shadow cards
-- dealt to it and each player must discard an ally he controls. Checked as each is dealt.
-- Shadow: Either raise your threat by 4, or discard an ally you control.
instance RunMessage ThaurdirsElite where
  runMessage msg e@(ThaurdirsElite attrs) = case msg of
    AttachShadowCard eid _ | eid == attrs.id -> do
      attrs' <- runMessage msg attrs
      when (length attrs'.shadowCards >= 3) do
        for_ attrs'.shadowCards (discardShadowCard attrs.id)
        eachPlayer \p -> do
          allies <- select (AllyControlledBy $ PlayerWithId p)
          chooseOrRunOneM p do
            questionLabeled "thaurdirsElite.discardAlly"
            causedBy attrs
            targets allies (discard attrs)
      pure $ ThaurdirsElite attrs'
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "thaurdirsElite.choose"
        causedBy attrs
        labeled "thaurdirsElite.threat" $ encounterRaisesThreat pid 4
        unless (null allies) $ labeled "thaurdirsElite.ally" do
          chooseOrRunOneM pid do
            questionLabeled "thaurdirsElite.discardAlly"
            causedBy attrs
            targets allies (discard attrs)
      pure e
    _ -> ThaurdirsElite <$> runMessage msg attrs
