module LordOfTheRings.Attachment.Cards.ExtraSails (extraSails, extraSailsUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ExtraSails = ExtraSails AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Modification boon.
extraSails :: AttachmentCard ExtraSails
extraSails = attachment ExtraSails Cards.extraSails & attachTo (AttachToAlly shipObjective)

extraSailsUpgraded :: AttachmentCard ExtraSails
extraSailsUpgraded = attachment ExtraSails Cards.extraSailsUpgraded & attachTo (AttachToAlly shipObjective)

-- | Attached Ship gets +1 willpower.
instance HasModifiersFor ExtraSails where
  getModifiersFor (ExtraSails a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Upgraded: Response: When a non-unique Ship enemy engages you, spend 1 resource and
-- exhaust Extra Sails to reduce that enemy's Boarding value by 1.
instance HasAbilities ExtraSails where
  getAbilities (ExtraSails a) =
    [ restricted a 1 (exists $ HeroControlledBy (PlayerWithId a.controller) <> HeroWithResources) $ ResponseAbility (AnyEnemyEngagedWindow When) (exhaustCost a)
    | isUpgradedSide a
    ]

instance RunMessage ExtraSails where
  runMessage msg s@(ExtraSails a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      ships <- select (shipEnemy <> not_ UniqueEnemy)
      for_ [eid | Window _ (EnemyEngaged eid p) <- windows, p == pid, eid `elem` ships] \eid -> do
        spendResourceFromAHero pid a
        phaseModifier a eid (BoardingModifier (-1))
      pure s
    _ -> ExtraSails <$> removedAsItLeaves msg a
