module LordOfTheRings.Attachment.Cards.SecretVigil (secretVigil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)

newtype SecretVigil = SecretVigil AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to an enemy. Limit 1 per enemy, on its card def.
secretVigil :: AttachmentCard SecretVigil
secretVigil = attachment SecretVigil Cards.secretVigil & attachTo (AttachToEnemy AnyEnemy)

-- | Attached enemy gets -1 threat.
instance HasModifiersFor SecretVigil where
  getModifiersFor (SecretVigil a) = withHost a \host -> modified a host [ThreatModifier (-1)]

-- | Response: When attached enemy is destroyed, reduce each player's threat by the
-- attached enemy's printed threat.
instance HasAbilities SecretVigil where
  getAbilities (SecretVigil a) =
    [mkAbility a 1 $ response $ ThisEnemyDefeatedWindow When eid | AttachedTo (EnemyTarget eid) <- [a.placement]]

instance RunMessage SecretVigil where
  runMessage msg s@(SecretVigil a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [eid | Window _ (EnemyDefeated eid) <- windows]) \eid -> do
        n <- fromMaybe 0 . cdThreat . toCardDef <$> getEnemy eid
        eachPlayer \pid -> lowerThreatBy a pid n
      pure s
    _ -> SecretVigil <$> runMessage msg a
