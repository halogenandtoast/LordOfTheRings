module LordOfTheRings.Attachment.Cards.ToTheSeaToTheSea (toTheSeaToTheSea) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Noldor))

newtype ToTheSeaToTheSea = ToTheSeaToTheSea AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor character.
toTheSeaToTheSea :: AttachmentCard ToTheSeaToTheSea
toTheSeaToTheSea =
  attachment ToTheSeaToTheSea Cards.toTheSeaToTheSea
    & attachTo (AttachToCharacterMatching (HeroWithTrait Noldor) (AllyWithTrait Noldor))

-- | Action: Exhaust To the Sea, to the Sea! and discard X cards from your hand to reduce
-- the cost of the next Noldor ally played this phase by X (to a minimum of 1).
instance HasAbilities ToTheSeaToTheSea where
  getAbilities (ToTheSeaToTheSea a) =
    [ restricted a 1 (PlayerExists $ PlayerWithId a.controller <> PlayerWithCardsInHand 1)
        $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

-- | On the game, as O Lórien!'s: whichever player plays the next one.
instance RunMessage ToTheSeaToTheSea where
  runMessage msg s@(ToTheSeaToTheSea a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      hand <- length . (.hand) <$> getPlayer pid
      chooseAmountM pid "toTheSeaToTheSea.discard" a 1 hand \x -> do
        chooseAndDiscardFromHand pid a x
        phaseModifier a GameTarget (ReduceNextCostOf (CardWithType AllyType <> CardWithTrait Noldor) x)
      pure s
    _ -> ToTheSeaToTheSea <$> runMessage msg a
