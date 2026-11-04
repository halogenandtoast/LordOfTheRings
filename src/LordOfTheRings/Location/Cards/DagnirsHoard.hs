module LordOfTheRings.Location.Cards.DagnirsHoard (dagnirsHoard) where

import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers
import LordOfTheRings.Trait (Trait (Item))

newtype DagnirsHoard = DagnirsHoard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dagnirsHoard :: LocationCard DagnirsHoard
dagnirsHoard = location DagnirsHoard Cards.dagnirsHoard

-- | Forced: When Dagnir's Hoard is explored, Dagnir returns to the staging area and makes
-- an immediate attack against each player in turn order.
-- Response: After Dagnir's Hoard is explored, each player may search his deck for an Item
-- attachment, add it to his hand, and shuffle his deck.
instance HasAbilities DagnirsHoard where
  getAbilities (DagnirsHoard a) =
    [ mkAbility a 1 $ forced $ ExploredWindow When a.id
    , mkAbility a 2 $ response $ ExploredWindow When a.id
    ]

instance RunMessage DagnirsHoard where
  runMessage msg l@(DagnirsHoard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnDagnirToStaging
      getDagnir >>= traverse_ \dagnir -> eachPlayer \p -> immediateAttack p dagnir
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer \p -> do
        deck <- (.deck) <$> getPlayer p
        searchDeck p a (length deck) (CardWithType AttachmentType <> CardWithTrait Item) False
      pure l
    _ -> DagnirsHoard <$> runMessage msg a
