module LordOfTheRings.Contract.Cards.ForthTheThreeHunters (forthTheThreeHunters) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Attachment.Types (AttachmentAttrs (attachmentCardId))
import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Contract.CardDefs qualified as Cards
import LordOfTheRings.Contract.Import.Lifted
import LordOfTheRings.Game.Base (gameEntities, gameRound)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype ForthTheThreeHunters = ForthTheThreeHunters ContractAttrs
  deriving anyclass IsContract
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Side A: Your deck cannot include ally cards (checked as the deck is chosen,
-- 'LordOfTheRings.Decklist').
forthTheThreeHunters :: ContractCard ForthTheThreeHunters
forthTheThreeHunters = contract ForthTheThreeHunters Cards.forthTheThreeHunters

restrictedAttachment :: CardMatcher
restrictedAttachment = CardWithType AttachmentType <> CardWithKeyword Restricted

-- | The heroes a restricted attachment was played on this round, and the round, in the
-- meta.
playedOn :: ContractAttrs -> (Int, [HeroId])
playedOn a = fromMaybe (0, []) $ parseMaybe parseJSON a.meta

{- | Side A: Each of your heroes can have 1 additional restricted attachment. Reduce the
cost of the first restricted attachment you play on each of your heroes each round by 1.
Side B: Each of your heroes can have 1 additional restricted attachment and gets +1
willpower for each restricted attachment on it.
-}
instance HasModifiersFor ForthTheThreeHunters where
  getModifiersFor (ForthTheThreeHunters a) = do
    heroes <- select (HeroControlledBy $ PlayerWithId a.owner)
    round' <- getsGame gameRound
    let (r, done) = playedOn a
    for_ heroes \h -> do
      modified a (HeroTarget h) [AdditionalRestrictedSlots 1]
      if a.flipped
        then do
          n <- restrictedAttachmentsOn (HeroTarget h)
          when (n > 0) $ modified a (HeroTarget h) [WillpowerModifier n]
        else unless (r == round' && h `elem` done) $ modified a (HeroTarget h) [ReduceCostToAttach restrictedAttachment 1]

{- | Side A: Refresh Action: If each of your heroes has at least 2 restricted attachments,
flip this card over.
Side B: Action: Exhaust this card to heal 1 damage from each of your heroes.
-}
instance HasAbilities ForthTheThreeHunters where
  getAbilities (ForthTheThreeHunters a)
    | a.flipped = [mkAbility a 2 $ ActionAbility $ exhaustCost a | not a.exhausted]
    | otherwise =
        [ restricted a 1 (DuringPhase RefreshPhase <> NotCriterion (exists $ HeroControlledBy you <> NotHero (HeroWithRestrictedAttachmentsAtLeast 2)))
            $ ActionAbility Free
        ]
   where
    you = PlayerWithId a.owner

instance RunMessage ForthTheThreeHunters where
  runMessage msg c@(ForthTheThreeHunters attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      flipOver attrs
      pure c
    UseCardAbility pid (isSource attrs -> True) 2 _ -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> healDamage attrs (HeroTarget h) 1
      pure c
    -- the hero a restricted attachment was just played on has had its reduction this round
    CheckWindows ws | not attrs.flipped -> do
      attachments <- getsGame (toList . entitiesAttachments . gameEntities)
      round' <- getsGame gameRound
      heroes <- select (HeroControlledBy $ PlayerWithId attrs.owner)
      let played =
            [ h
            | Window After (PlayedCard pid card) <- ws
            , pid == attrs.owner
            , cardMatches restrictedAttachment card
            , att <- attachments
            , (toAttrs att).attachmentCardId == card.id
            , AttachedTo (HeroTarget h) <- [att.placement]
            , h `elem` heroes
            ]
          (r, done) = playedOn attrs
          earlier = if r == round' then done else []
      pure
        $ if null played
          then c
          else ForthTheThreeHunters $ attrs {contractMeta = toJSON (round', ordNub $ earlier <> played)}
    _ -> ForthTheThreeHunters <$> runMessage msg attrs
