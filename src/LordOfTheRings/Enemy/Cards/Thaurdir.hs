module LordOfTheRings.Enemy.Cards.Thaurdir (thaurdirCaptain, thaurdirChampion) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Sorcery))

newtype Thaurdir = Thaurdir EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | One card with two sides: flipping him turns his card code to the other side's, which
-- gives his stats, traits and text.
thaurdirCaptain :: EnemyCard Thaurdir
thaurdirCaptain = enemy Thaurdir Cards.thaurdirCaptain

thaurdirChampion :: EnemyCard Thaurdir
thaurdirChampion = enemy Thaurdir Cards.thaurdirChampion

isChampion :: EnemyAttrs -> Bool
isChampion a = toCardCode a == toCardCode Cards.thaurdirChampion

-- | Cannot have attachments.
-- Captain: Each other enemy has +10 engagement cost.
-- Champion: Each other enemy has -10 engagement cost.
-- Both stages keep him in the staging area: he makes no engagement checks and cannot be
-- engaged.
instance HasModifiersFor Thaurdir where
  getModifiersFor (Thaurdir a) = do
    modifySelf a [CannotHaveAttachments, NoEngagementChecks, CannotBeOptionallyEngagedBy AnyPlayer]
    others <- select (not_ $ EnemyWithId a.id)
    for_ others \e -> modified a e [EngagementCostModifier (if isChampion a then -10 else 10)]

-- | Forced: After Thaurdir is flipped or a treachery card with the Sorcery trait is
-- revealed, ...
instance HasAbilities Thaurdir where
  getAbilities (Thaurdir a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Sorcery]

instance RunMessage Thaurdir where
  runMessage msg e@(Thaurdir attrs) = case msg of
    -- he cannot leave the staging area
    PlaceEnemy eid placement | eid == attrs.id && placement /= StagingArea -> pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      forcedEffect attrs
      pure e
    Flip (isTarget attrs -> True) -> do
      let other = if isChampion attrs then Cards.thaurdirCaptain else Cards.thaurdirChampion
          flipped = attrs & cardCodeL .~ toCardCode other
      forcedEffect flipped
      pure $ Thaurdir flipped
    _ -> Thaurdir <$> runMessage msg attrs

{- | Captain: deal 1 shadow card to each enemy in play.
Champion: Thaurdir heals 3 damage and makes an immediate attack against the first player.
-}
forcedEffect :: ReverseQueue m => EnemyAttrs -> m ()
forcedEffect a
  | isChampion a = do
      healDamage a a 3
      lead <- getLead
      immediateAttack lead a.id
  | otherwise = traverse_ dealShadowCard =<< select AnyEnemy
