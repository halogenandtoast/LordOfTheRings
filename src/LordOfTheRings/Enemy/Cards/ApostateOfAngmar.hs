module LordOfTheRings.Enemy.Cards.ApostateOfAngmar (apostateOfAngmar) where

import LordOfTheRings.Card (cdThreat, toCardCode, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ApostateOfAngmar = ApostateOfAngmar EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

apostateOfAngmar :: EnemyCard ApostateOfAngmar
apostateOfAngmar = enemy ApostateOfAngmar Cards.apostateOfAngmar

-- | While you are engaged with Apostate of Angmar, you cannot attack Thaurdir.
instance HasModifiersFor ApostateOfAngmar where
  getModifiersFor (ApostateOfAngmar a) = case a.placement of
    EngagedWith pid -> do
      thaurdir <- select (EnemyWithCardCode $ toCardCode Cards.thaurdirTheTreacheryOfRhudaur)
      for_ thaurdir \t -> modified a t [CannotBeAttackedByCharactersOf pid]
    _ -> pure ()

-- | Shadow: Discard the top card of the encounter deck. Attacking enemy gets +X attack,
-- where X is that card's printed threat.
instance RunMessage ApostateOfAngmar where
  runMessage msg e@(ApostateOfAngmar attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      top <- getTopOfEncounterDeck
      discardTopOfEncounterDeck attrs 1
      let x = maybe 0 (fromMaybe 0 . cdThreat . toCardDef) top
      when (x > 0) $ enemyGetsAttack (CardIdSource cid) eid x
      pure e
    _ -> ApostateOfAngmar <$> runMessage msg attrs
