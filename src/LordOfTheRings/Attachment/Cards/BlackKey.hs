module LordOfTheRings.Attachment.Cards.BlackKey (blackKey, blackKeyUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Ally.Types (allyTokens)
import LordOfTheRings.Classes.HasGame (HasGame)

newtype BlackKey = BlackKey AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The boon side of Black Key, attached to whom the campaign card's setup says: Calphon,
-- or Captain Sahír.
blackKey :: AttachmentCard BlackKey
blackKey = attachment BlackKey Cards.blackKey & attachTo AttachToCharacter

blackKeyUpgraded :: AttachmentCard BlackKey
blackKeyUpgraded = attachment BlackKey Cards.blackKeyUpgraded & attachTo AttachToCharacter

-- | Attached character gets +1 willpower, +1 attack and +1 defense.
instance HasModifiersFor BlackKey where
  getModifiersFor (BlackKey a) = withHost a \host -> modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

-- | Upgraded: Response: After attached character quests successfully, place 1 resource on
-- them. Action: Exhaust Black Key and remove 3 resources from attached character to ready
-- them.
instance HasAbilities BlackKey where
  getAbilities (BlackKey a) =
    concat
      [ [ restricted a 1 (committed host) $ response $ QuestedSuccessfullyWindow After
        , mkAbility a 2 $ ActionAbility (exhaustCost a)
        ]
      | isUpgradedSide a
      , AttachedTo host <- [a.placement]
      ]

instance RunMessage BlackKey where
  runMessage msg s@(BlackKey a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      withHost a \host -> do
        n <- resourcesOn host
        when (n >= 3) do
          removeTokens a host Resource 3
          ready host
      pure s
    _ -> BlackKey <$> removedAsItLeaves msg a

committed :: Target -> Criterion
committed = \case
  HeroTarget hid -> exists (HeroWithId hid <> CommittedHero)
  AllyTarget aid -> exists (AllyWithId aid <> CommittedAlly)
  _ -> NotCriterion NoCriteria

resourcesOn :: HasGame m => Target -> m Int
resourcesOn = \case
  HeroTarget hid -> (.resources) <$> getHero hid
  AllyTarget aid -> countTokens Resource . allyTokens . toAttrs <$> getAlly aid
  _ -> pure 0
