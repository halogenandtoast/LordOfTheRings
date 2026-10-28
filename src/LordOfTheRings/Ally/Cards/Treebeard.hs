module LordOfTheRings.Ally.Cards.Treebeard (treebeard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Trait (Trait (Ent))

newtype Treebeard = Treebeard AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Treebeard enters play exhausted.
treebeard :: AllyCard Treebeard
treebeard = ally (Treebeard . (exhaustedL .~ True)) Cards.treebeard

-- | Cannot have restricted attachments. Treebeard collects 1 resource each resource
-- phase. These resources can be used to pay for Ent cards played from your hand.
instance HasModifiersFor Treebeard where
  getModifiersFor (Treebeard a) = modified a a [CannotHaveRestrictedAttachments, ResourcesPayFor (CardWithTrait Ent)]

-- | Action: Pay 2 resources from Treebeard's pool to ready an Ent character.
instance HasAbilities Treebeard where
  getAbilities (Treebeard a) =
    [ restricted a 1 (CharactersAtLeast 1 (HeroWithTrait Ent <> ExhaustedHero) (AllyWithTrait Ent <> ExhaustedAlly))
        $ ActionAbility (ResourceCost (toTarget a) 2)
    ]

instance RunMessage Treebeard where
  runMessage msg s@(Treebeard a) = case msg of
    BeginPhase ResourcePhase -> do
      placeResources a a 1
      Treebeard <$> runMessage msg a
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      ents <- selectCharacters (HeroWithTrait Ent <> ExhaustedHero) (AllyWithTrait Ent <> ExhaustedAlly)
      chooseOrRunOneM pid do
        questionLabeled "treebeard.ready"
        causedBy a
        targets ents ready
      pure s
    _ -> Treebeard <$> runMessage msg a
