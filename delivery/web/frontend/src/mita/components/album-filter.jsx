import React, { useState } from "react";
import Modal from "react-modal";

function Condition(props) {
  function renderInput(item) {
    if (item.op.value === "like") {
      return (
        <input
          value={item.input.value}
          onChange={(e) => props.onChangeInputValue(item.key, e.target.value)}
        />
      );
    }
    return null;
  }

  const itemEls = Object.values(props.condition)
    .sort((a, b) => a.order - b.order)
    .map((item) => {
      return (
        <li key={item.key}>
          <select
            value={item.field.value}
            onChange={(e) => props.onChangeField(item.key, e.target.value)}
          >
            {item.field.options.map((o) => {
              return <option value={o.value}>{o.name}</option>;
            })}
          </select>

          <select
            value={item.op.value}
            onChange={(e) => props.onChangeOp(item.key, e.target.value)}
          >
            {item.op.options.map((o) => {
              return <option value={o.value}>{o.name}</option>;
            })}
          </select>

          {renderInput(item)}
        </li>
      );
    });

  return (
    <div>
      <button onClick={props.onAddItem}>Add</button>
      <ul>{itemEls}</ul>
    </div>
  );
}

function Sort(props) {
  return (
    <div>
      <div>
        <select
          value={props.field}
          onChange={(e) => props.onChangeField(e.target.value)}
        >
          <option value="name">Name</option>
          <option value="created_on">Created on</option>
        </select>
      </div>
      <div>
        <select
          value={props.direction}
          onChange={(e) => props.onChangeDirection(e.target.value)}
        >
          <option value="asc">asc</option>
          <option value="desc">desc</option>
        </select>
      </div>
    </div>
  );
}

let itemCounter = 0;

export default function AlbumFilterModal(props) {
  const [condition, setCondition] = useState({});
  function handleAddConditionItem() {
    const key = itemCounter++;
    const obj = {};
    obj[key] = {
      key: key,
      order: key,
      field: {
        value: "name",
        options: [
          {
            name: "Name",
            value: "name",
          },
        ],
      },
      op: {
        value: "like",
        options: [{ name: "Like", value: "like" }],
      },
      input: {
        value: "",
      },
    };
    setCondition((cond) => Object.assign({}, cond, obj));
  }
  function handleChangeConditionField(itemKey, value) {
    setCondition((cond) => {
      const item = cond[itemKey];
      if (!item) {
        return cond;
      }
      const obj = {};
      obj[itemKey] = Object.assign({}, item);
      obj[itemKey].field.value = value;
      obj[itemKey].input.value = "";
      return Object.assign({}, cond, obj);
    });
  }

  function handleChangeConditionOp(itemKey, value) {
    setCondition((cond) => {
      const item = cond[itemKey];
      if (!item) {
        return cond;
      }
      const obj = {};
      obj[itemKey] = Object.assign({}, item);
      obj[itemKey].op.value = value;
      return Object.assign({}, cond, obj);
    });
  }
  function handleChangeConditionInputValue(itemKey, value) {
    setCondition((cond) => {
      const item = cond[itemKey];
      if (!item) {
        return cond;
      }
      const obj = {};
      obj[itemKey] = Object.assign({}, item);
      obj[itemKey].input.value = value;
      return Object.assign({}, cond, obj);
    });
  }

  const [sort, setSort] = useState({
    field: "name",
    direction: "desc",
  });

  function handleChangeSortField(field) {
    setSort((s) => Object.assign({}, s, { field: field }));
  }

  function handleChangeSortDirection(dir) {
    setSort((s) => Object.assign({}, s, { direction: dir }));
  }

  return (
    <Modal isOpen={true} onRequestClose={props.onClose}>
      <div>
        <span>Condition</span>
        <Condition
          condition={condition}
          onAddItem={handleAddConditionItem}
          onChangeField={handleChangeConditionField}
          onChangeOp={handleChangeConditionOp}
          onChangeInputValue={handleChangeConditionInputValue}
        />
      </div>

      <div>
        <span>Sort</span>
        <Sort
          {...sort}
          onChangeField={handleChangeSortField}
          onChangeDirection={handleChangeSortDirection}
        />
      </div>

      <div className="modal-footer">
        <div className="form-row align-items-center">
          <div className="col-auto">
            <button className="btn" onClick={props.onClose}>
              Cancel
            </button>
          </div>
          <div className="col-auto">
            <button
              className="btn btn-primary"
              onClick={() =>
                props.onApply({
                  condition: Object.values(condition)
                    .sort((a, b) => a.order - b.order)
                    .map((item) => {
                      return {
                        field: item.field.value,
                        op: item.op.value,
                        input: item.input.value,
                      };
                    }),
                  sort,
                })
              }
            >
              Apply
            </button>
          </div>
        </div>
      </div>
    </Modal>
  );
}
